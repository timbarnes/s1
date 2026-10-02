//! `syntax-rules` transformers (R7RS 4.3.2): parsing and validation,
//! pattern matching, and template instantiation with renaming.
//!
//! This is plain Rust over heap data: expanding a use runs no Scheme code.
//! Hygiene comes from renaming every identifier a template introduces to an
//! alias (see `eval::identifiers` and Docs/hygiene-design.md); the evaluator
//! then evaluates the expansion in the use environment.

use crate::env::{EnvOps, EnvRef};
use crate::eval::identifiers::{free_identifier_eq, lookup, strip, strip_datum};
use crate::gc::{GcHeap, GcRef, SchemeValue, equal, new_pair, new_vector};
use crate::gc_value;
use crate::printer::print_value;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

/// A transformer made by evaluating a `syntax-rules` form.
pub struct SyntaxRules {
    /// The custom ellipsis identifier, if one was given
    pub ellipsis: Option<GcRef>,
    pub literals: Vec<GcRef>,
    /// (pattern, template) pairs, in order
    pub rules: Vec<(GcRef, GcRef)>,
    /// The definition environment: where introduced identifiers resolve
    pub env: EnvRef,
}

impl std::fmt::Debug for SyntaxRules {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SyntaxRules({} rules)", self.rules.len())
    }
}

impl crate::gc::Mark for SyntaxRules {
    fn mark(&self, visit: &mut dyn FnMut(GcRef)) {
        if let Some(e) = self.ellipsis {
            visit(e);
        }
        for l in &self.literals {
            visit(*l);
        }
        for (p, t) in &self.rules {
            visit(*p);
            visit(*t);
        }
        self.env.mark(visit);
    }
}

/// What a pattern variable matched: one form, or a sequence per ellipsis.
#[derive(Clone, Debug)]
enum Binding {
    One(GcRef),
    Many(Vec<Binding>),
}

type Bindings = HashMap<GcRef, Binding>;

fn is_identifier(v: GcRef) -> bool {
    matches!(gc_value!(v), SchemeValue::Symbol(_))
}

fn symbol_name(v: GcRef) -> Option<&'static str> {
    match gc_value!(v) {
        SchemeValue::Symbol(s) => Some(s.as_str()),
        _ => None,
    }
}

/// The proper elements of a (possibly improper) list, and its final cdr.
fn list_parts(list: GcRef) -> (Vec<GcRef>, GcRef) {
    let mut items = Vec::new();
    let mut rest = list;
    while let SchemeValue::Pair(car, cdr) = gc_value!(rest) {
        items.push(*car);
        rest = *cdr;
    }
    (items, rest)
}

impl SyntaxRules {
    fn is_literal(&self, id: GcRef) -> bool {
        self.literals.contains(&id)
    }

    /// Whether `id` is the ellipsis. Literals take priority (R7RS 4.3.2). A
    /// custom ellipsis is matched by identity; the default matches any
    /// identifier that strips to `...`, so a `...` made by an outer
    /// template's `(... ...)` escape (an alias) works in an inner macro.
    fn is_ellipsis(&self, heap: &GcHeap, id: GcRef) -> bool {
        if !is_identifier(id) || self.is_literal(id) {
            return false;
        }
        match self.ellipsis {
            Some(e) => id == e,
            None => symbol_name(strip(heap, id)) == Some("..."),
        }
    }

    fn is_underscore(&self, heap: &GcHeap, id: GcRef) -> bool {
        !self.is_literal(id) && symbol_name(strip(heap, id)) == Some("_")
    }

    // -----------------------------------------------------------------------
    // Expansion
    // -----------------------------------------------------------------------

    /// Expand the use `form`, which appears in `use_env`.
    pub fn expand(&self, heap: &mut GcHeap, form: GcRef, use_env: &EnvRef) -> Result<GcRef, String> {
        let input = match gc_value!(form) {
            SchemeValue::Pair(_, rest) => *rest,
            _ => return Err("syntax-rules: macro use is not a list".to_string()),
        };
        for &(pattern, template) in &self.rules {
            let pattern_rest = match gc_value!(pattern) {
                SchemeValue::Pair(_, rest) => *rest,
                _ => continue,
            };
            let mut binds = Bindings::default();
            if self.match_pattern(heap, pattern_rest, input, use_env, &mut binds) {
                let mut renames = HashMap::default();
                let expansion = self.instantiate(heap, template, &binds, &mut renames, false)?;
                return Ok(strip_literal_data(heap, expansion, use_env));
            }
        }
        Err(format!("no syntax rule matches {}", print_value(&form)))
    }

    fn match_pattern(
        &self,
        heap: &GcHeap,
        pattern: GcRef,
        input: GcRef,
        use_env: &EnvRef,
        binds: &mut Bindings,
    ) -> bool {
        match gc_value!(pattern) {
            SchemeValue::Symbol(_) => {
                if self.is_literal(pattern) {
                    is_identifier(input)
                        && free_identifier_eq(heap, pattern, &self.env, input, use_env)
                } else if self.is_underscore(heap, pattern) {
                    true
                } else {
                    binds.insert(pattern, Binding::One(input));
                    true
                }
            }
            SchemeValue::Pair(..) => {
                let (items, tail) = list_parts(pattern);
                if !items.iter().any(|p| self.is_ellipsis(heap, *p)) {
                    return self.match_pairs(heap, pattern, input, use_env, binds);
                }
                let (inputs, input_tail) = list_parts(input);
                self.match_sequence(heap, &items, tail, &inputs, input_tail, use_env, binds)
            }
            SchemeValue::Vector(items) => match gc_value!(input) {
                SchemeValue::Vector(inputs) => {
                    let nil = heap.nil_s();
                    self.match_sequence(heap, items, nil, inputs, nil, use_env, binds)
                }
                _ => false,
            },
            SchemeValue::Nil => matches!(gc_value!(input), SchemeValue::Nil),
            _ => equal(heap, pattern, input),
        }
    }

    /// Match a list pattern with no ellipsis, pair by pair; a dotted tail in
    /// the pattern matches whatever input remains (`(a . rest)`).
    fn match_pairs(
        &self,
        heap: &GcHeap,
        mut pattern: GcRef,
        mut input: GcRef,
        use_env: &EnvRef,
        binds: &mut Bindings,
    ) -> bool {
        loop {
            match (gc_value!(pattern), gc_value!(input)) {
                (SchemeValue::Pair(p, prest), SchemeValue::Pair(i, irest)) => {
                    if !self.match_pattern(heap, *p, *i, use_env, binds) {
                        return false;
                    }
                    pattern = *prest;
                    input = *irest;
                }
                (SchemeValue::Pair(..), _) => return false,
                (SchemeValue::Nil, _) => return matches!(gc_value!(input), SchemeValue::Nil),
                _ => return self.match_pattern(heap, pattern, input, use_env, binds),
            }
        }
    }

    /// Match pattern elements `items` (with final cdr `tail`) against input
    /// elements `inputs` (with final cdr `input_tail`): a list pattern
    /// containing an ellipsis, or any vector pattern. The element before the
    /// ellipsis takes as many inputs as leave enough for the patterns after
    /// it; a non-nil `tail` then matches the input's final cdr.
    #[allow(clippy::too_many_arguments)]
    fn match_sequence(
        &self,
        heap: &GcHeap,
        items: &[GcRef],
        tail: GcRef,
        inputs: &[GcRef],
        input_tail: GcRef,
        use_env: &EnvRef,
        binds: &mut Bindings,
    ) -> bool {
        let ellipsis_at = items.iter().position(|&p| self.is_ellipsis(heap, p));
        let (before, repeated, after) = match ellipsis_at {
            // `parse` rejects a leading ellipsis; refuse rather than panic.
            Some(0) => return false,
            Some(i) => (&items[..i - 1], Some(items[i - 1]), &items[i + 1..]),
            None => (items, None, &items[items.len()..]),
        };
        let has_tail = !matches!(gc_value!(tail), SchemeValue::Nil);

        let fixed = before.len() + after.len();
        let reps = match repeated {
            Some(_) => match inputs.len().checked_sub(fixed) {
                Some(n) => n,
                None => return false,
            },
            // Only vectors get here without an ellipsis.
            None if inputs.len() != before.len() => return false,
            None => 0,
        };

        for (p, i) in before.iter().zip(inputs) {
            if !self.match_pattern(heap, *p, *i, use_env, binds) {
                return false;
            }
        }
        if let Some(sub) = repeated {
            let mut matches = Vec::with_capacity(reps);
            for i in &inputs[before.len()..before.len() + reps] {
                let mut b = Bindings::default();
                if !self.match_pattern(heap, sub, *i, use_env, &mut b) {
                    return false;
                }
                matches.push(b);
            }
            for var in self.pattern_vars(heap, sub) {
                let seq = matches
                    .iter_mut()
                    .map(|b| b.remove(&var).expect("pattern variable bound by match"))
                    .collect();
                binds.insert(var, Binding::Many(seq));
            }
            let rest = &inputs[before.len() + reps..];
            for (p, i) in after.iter().zip(rest) {
                if !self.match_pattern(heap, *p, *i, use_env, binds) {
                    return false;
                }
            }
        }

        if has_tail {
            self.match_pattern(heap, tail, input_tail, use_env, binds)
        } else {
            matches!(gc_value!(input_tail), SchemeValue::Nil)
        }
    }

    /// The pattern variables in `pattern`, in order.
    fn pattern_vars(&self, heap: &GcHeap, pattern: GcRef) -> Vec<GcRef> {
        let mut vars = Vec::new();
        self.collect_pattern_vars(heap, pattern, 0, &mut |v, _| vars.push(v));
        vars
    }

    fn collect_pattern_vars(&self, heap: &GcHeap, pattern: GcRef, depth: usize, out: &mut dyn FnMut(GcRef, usize)) {
        match gc_value!(pattern) {
            SchemeValue::Symbol(_) => {
                if !self.is_literal(pattern) && !self.is_underscore(heap, pattern) && !self.is_ellipsis(heap, pattern) {
                    out(pattern, depth);
                }
            }
            SchemeValue::Pair(..) | SchemeValue::Vector(_) => {
                let (items, tail) = match gc_value!(pattern) {
                    SchemeValue::Vector(items) => (items.clone(), heap.nil_s()),
                    _ => list_parts(pattern),
                };
                for (i, item) in items.iter().enumerate() {
                    let repeated = items.get(i + 1).is_some_and(|n| self.is_ellipsis(heap, *n));
                    self.collect_pattern_vars(heap, *item, depth + repeated as usize, out);
                }
                self.collect_pattern_vars(heap, tail, depth, out);
            }
            _ => {}
        }
    }

    /// Build the expansion of `template` under `binds`. `escaped` is true
    /// inside `(... template)`, where the ellipsis is an ordinary identifier.
    fn instantiate(
        &self,
        heap: &mut GcHeap,
        template: GcRef,
        binds: &Bindings,
        renames: &mut HashMap<GcRef, GcRef>,
        escaped: bool,
    ) -> Result<GcRef, String> {
        match gc_value!(template) {
            SchemeValue::Symbol(_) => match binds.get(&template) {
                Some(Binding::One(v)) => Ok(*v),
                Some(Binding::Many(_)) => Err(format!(
                    "syntax-rules: pattern variable {} used without an ellipsis",
                    print_value(&template)
                )),
                None => Ok(*renames
                    .entry(template)
                    .or_insert_with(|| heap.make_alias(template, self.env.clone()))),
            },
            SchemeValue::Pair(head, rest) => {
                let (head, rest) = (*head, *rest);
                // (... template): the template with the ellipsis escaped
                if !escaped && self.is_ellipsis(heap, head) {
                    if let SchemeValue::Pair(inner, _) = gc_value!(rest) {
                        return self.instantiate(heap, *inner, binds, renames, true);
                    }
                }
                let (items, tail) = list_parts(template);
                let mut out = Vec::new();
                let mut i = 0;
                while i < items.len() {
                    let item = items[i];
                    let mut depth = 0;
                    while !escaped
                        && items.get(i + 1 + depth).is_some_and(|n| self.is_ellipsis(heap, *n))
                    {
                        depth += 1;
                    }
                    if depth == 0 {
                        out.push(self.instantiate(heap, item, binds, renames, escaped)?);
                    } else {
                        self.instantiate_repeated(heap, item, binds, renames, depth, &mut out)?;
                    }
                    i += 1 + depth;
                }
                let mut result = self.instantiate(heap, tail, binds, renames, escaped)?;
                for item in out.into_iter().rev() {
                    result = new_pair(heap, item, result);
                }
                Ok(result)
            }
            SchemeValue::Vector(items) => {
                let list = crate::gc::list_from_slice(&items.clone(), heap);
                let expanded = self.instantiate(heap, list, binds, renames, escaped)?;
                let (elems, _) = list_parts(expanded);
                Ok(new_vector(heap, elems))
            }
            _ => Ok(template),
        }
    }

    /// Instantiate `sub ...` (with `depth` ellipses), appending each result.
    fn instantiate_repeated(
        &self,
        heap: &mut GcHeap,
        sub: GcRef,
        binds: &Bindings,
        renames: &mut HashMap<GcRef, GcRef>,
        depth: usize,
        out: &mut Vec<GcRef>,
    ) -> Result<(), String> {
        let mut vars = Vec::new();
        template_vars(sub, binds, &mut vars, &mut HashSet::default());
        let seqs: Vec<(GcRef, &Vec<Binding>)> = vars
            .iter()
            .filter_map(|v| match binds.get(v) {
                Some(Binding::Many(seq)) => Some((*v, seq)),
                _ => None,
            })
            .collect();
        let len = match seqs.first() {
            Some((_, seq)) => seq.len(),
            None => {
                return Err(format!(
                    "syntax-rules: no pattern variable to repeat in {}",
                    print_value(&sub)
                ));
            }
        };
        if seqs.iter().any(|(_, seq)| seq.len() != len) {
            return Err("syntax-rules: pattern variables repeat different numbers of times".to_string());
        }
        for i in 0..len {
            let mut inner = binds.clone();
            for (var, seq) in &seqs {
                inner.insert(*var, seq[i].clone());
            }
            if depth == 1 {
                out.push(self.instantiate(heap, sub, &inner, renames, false)?);
            } else {
                self.instantiate_repeated(heap, sub, &inner, renames, depth - 1, out)?;
            }
        }
        Ok(())
    }
}

/// The pattern variables (keys of `binds`) occurring in `template`.
fn template_vars(template: GcRef, binds: &Bindings, out: &mut Vec<GcRef>, seen: &mut HashSet<GcRef>) {
    match gc_value!(template) {
        SchemeValue::Symbol(_) => {
            if binds.contains_key(&template) && seen.insert(template) {
                out.push(template);
            }
        }
        SchemeValue::Pair(car, cdr) => {
            template_vars(*car, binds, out, seen);
            template_vars(*cdr, binds, out, seen);
        }
        SchemeValue::Vector(items) => {
            for i in items {
                template_vars(*i, binds, out, seen);
            }
        }
        _ => {}
    }
}

// ---------------------------------------------------------------------------
// Parsing and validation
// ---------------------------------------------------------------------------

/// Parse `(syntax-rules [ellipsis] (literal ...) (pattern template) ...)`,
/// evaluated in `env`, checking the rules are well formed.
pub fn parse(heap: &GcHeap, form: GcRef, env: EnvRef) -> Result<SyntaxRules, String> {
    let (parts, tail) = list_parts(form);
    if !matches!(gc_value!(tail), SchemeValue::Nil) || parts.len() < 2 {
        return Err("syntax-rules: expected (syntax-rules (literal ...) rule ...)".to_string());
    }
    let mut rest = &parts[1..];
    let ellipsis = if is_identifier(rest[0]) {
        let e = rest[0];
        rest = &rest[1..];
        Some(e)
    } else {
        None
    };
    let (literal_list, rules) = rest
        .split_first()
        .ok_or("syntax-rules: missing literal list")?;
    let (literals, lit_tail) = list_parts(*literal_list);
    if !matches!(gc_value!(lit_tail), SchemeValue::Nil) || !literals.iter().all(|l| is_identifier(*l)) {
        return Err("syntax-rules: literals must be a list of identifiers".to_string());
    }
    let mut sr = SyntaxRules {
        ellipsis,
        literals,
        rules: Vec::new(),
        env,
    };
    for rule in rules {
        let (pt, rule_tail) = list_parts(*rule);
        if pt.len() != 2 || !matches!(gc_value!(rule_tail), SchemeValue::Nil) {
            return Err(format!("syntax-rules: a rule must be (pattern template): {}", print_value(rule)));
        }
        let (pattern, template) = (pt[0], pt[1]);
        if !matches!(gc_value!(pattern), SchemeValue::Pair(..)) {
            return Err(format!("syntax-rules: a pattern must be a list: {}", print_value(&pattern)));
        }
        sr.check_pattern(heap, pattern)?;
        let mut depths = HashMap::default();
        sr.collect_pattern_vars(heap, cdr_of(pattern), 0, &mut |v, d| {
            depths.entry(v).or_insert(Vec::new()).push(d)
        });
        if let Some((v, _)) = depths.iter().find(|(_, ds)| ds.len() > 1) {
            return Err(format!("syntax-rules: pattern variable {} appears twice", print_value(v)));
        }
        let depths: HashMap<GcRef, usize> = depths.into_iter().map(|(v, ds)| (v, ds[0])).collect();
        sr.check_template(heap, template, &depths, 0, false)?;
        sr.rules.push((pattern, template));
    }
    Ok(sr)
}

fn cdr_of(pair: GcRef) -> GcRef {
    match gc_value!(pair) {
        SchemeValue::Pair(_, cdr) => *cdr,
        _ => pair,
    }
}

impl SyntaxRules {
    /// At most one ellipsis per list or vector level, and it must follow a
    /// pattern (the keyword position doesn't count).
    fn check_pattern(&self, heap: &GcHeap, pattern: GcRef) -> Result<(), String> {
        let check_level = |items: &[GcRef]| -> Result<(), String> {
            let positions: Vec<usize> = items
                .iter()
                .enumerate()
                .filter(|(_, p)| self.is_ellipsis(heap, **p))
                .map(|(i, _)| i)
                .collect();
            match positions.as_slice() {
                [] => Ok(()),
                [0] => Err(format!("syntax-rules: ellipsis must follow a pattern in {}", print_value(&pattern))),
                [_] => Ok(()),
                _ => Err(format!("syntax-rules: more than one ellipsis in {}", print_value(&pattern))),
            }
        };
        let walk = |p: GcRef| -> Result<(), String> {
            match gc_value!(p) {
                SchemeValue::Pair(..) => {
                    let (items, tail) = list_parts(p);
                    check_level(&items)?;
                    for i in items.iter().filter(|i| !self.is_ellipsis(heap, **i)) {
                        self.check_pattern_inner(heap, *i)?;
                    }
                    self.check_pattern_inner(heap, tail)
                }
                _ => Ok(()),
            }
        };
        // The keyword position is ignored entirely.
        walk(cdr_of(pattern))
    }

    fn check_pattern_inner(&self, heap: &GcHeap, p: GcRef) -> Result<(), String> {
        match gc_value!(p) {
            SchemeValue::Pair(..) | SchemeValue::Vector(_) => {
                let (items, tail) = match gc_value!(p) {
                    SchemeValue::Vector(items) => (items.clone(), heap.nil_s()),
                    _ => list_parts(p),
                };
                let ellipses: Vec<usize> = items
                    .iter()
                    .enumerate()
                    .filter(|(_, i)| self.is_ellipsis(heap, **i))
                    .map(|(i, _)| i)
                    .collect();
                if ellipses.len() > 1 {
                    return Err(format!("syntax-rules: more than one ellipsis in {}", print_value(&p)));
                }
                if ellipses.first() == Some(&0) {
                    return Err(format!("syntax-rules: ellipsis must follow a pattern in {}", print_value(&p)));
                }
                for i in items.iter().filter(|i| !self.is_ellipsis(heap, **i)) {
                    self.check_pattern_inner(heap, *i)?;
                }
                self.check_pattern_inner(heap, tail)
            }
            _ => Ok(()),
        }
    }

    /// Every pattern variable must be used under at least as many ellipses
    /// as it was matched under, and every ellipsis must follow something.
    fn check_template(
        &self,
        heap: &GcHeap,
        template: GcRef,
        depths: &HashMap<GcRef, usize>,
        depth: usize,
        escaped: bool,
    ) -> Result<(), String> {
        match gc_value!(template) {
            SchemeValue::Symbol(_) => match depths.get(&template) {
                Some(&d) if d > depth => Err(format!(
                    "syntax-rules: pattern variable {} needs {} ellipsis(es) in the template",
                    print_value(&template),
                    d
                )),
                _ => Ok(()),
            },
            SchemeValue::Pair(head, rest) => {
                if !escaped && self.is_ellipsis(heap, *head) {
                    return match gc_value!(*rest) {
                        SchemeValue::Pair(inner, after) if matches!(gc_value!(*after), SchemeValue::Nil) => {
                            self.check_template(heap, *inner, depths, depth, true)
                        }
                        _ => Err("syntax-rules: (... template) takes exactly one template".to_string()),
                    };
                }
                let (items, tail) = list_parts(template);
                let mut i = 0;
                while i < items.len() {
                    let mut n = 0;
                    while !escaped && items.get(i + 1 + n).is_some_and(|x| self.is_ellipsis(heap, *x)) {
                        n += 1;
                    }
                    self.check_template(heap, items[i], depths, depth + n, escaped)?;
                    i += 1 + n;
                }
                self.check_template(heap, tail, depths, depth, escaped)
            }
            SchemeValue::Vector(items) => {
                let list = items.clone();
                let mut i = 0;
                while i < list.len() {
                    let mut n = 0;
                    while !escaped && list.get(i + 1 + n).is_some_and(|x| self.is_ellipsis(heap, *x)) {
                        n += 1;
                    }
                    self.check_template(heap, list[i], depths, depth + n, escaped)?;
                    i += 1 + n;
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

// ---------------------------------------------------------------------------
// Literal data in expansions
// ---------------------------------------------------------------------------

/// Replace aliases with plain symbols inside the expansion's literal data:
/// the datum of each `quote` form, the quoted parts of each `quasiquote`
/// form, and vector literals. Code outside those keeps its aliases. Returns
/// `form` unchanged where nothing needed stripping.
fn strip_literal_data(heap: &mut GcHeap, form: GcRef, env: &EnvRef) -> GcRef {
    let quote = special_form(heap, env, "quote");
    let quasiquote = special_form(heap, env, "quasiquote");
    let mut walker = Stripper {
        env,
        quote,
        quasiquote,
        memo: HashMap::default(),
    };
    walker.code(heap, form)
}

/// The value bound to `name` in the global environment (the root of `env`).
fn special_form(heap: &mut GcHeap, env: &EnvRef, name: &str) -> Option<GcRef> {
    let mut global = env.clone();
    while let Some(parent) = global.parent() {
        global = parent;
    }
    let sym = heap.intern_symbol(name);
    global.lookup_local(sym)
}

struct Stripper<'a> {
    env: &'a EnvRef,
    quote: Option<GcRef>,
    quasiquote: Option<GcRef>,
    /// Results for pairs already walked, keyed by pair and context (0 for
    /// code, the quasiquote depth otherwise): shared structure is rewritten
    /// consistently and cyclic code can't loop.
    memo: HashMap<(GcRef, usize), GcRef>,
}

impl Stripper<'_> {
    /// Whether `head` names the special form `sf` where the code runs.
    fn names(&self, heap: &GcHeap, head: GcRef, sf: Option<GcRef>) -> bool {
        is_identifier(head) && sf.is_some() && lookup(heap, head, self.env) == sf
    }

    fn code(&mut self, heap: &mut GcHeap, form: GcRef) -> GcRef {
        if let Some(done) = self.memo.get(&(form, 0)) {
            return *done;
        }
        self.memo.insert((form, 0), form);
        let result = self.code_uncached(heap, form);
        self.memo.insert((form, 0), result);
        result
    }

    fn code_uncached(&mut self, heap: &mut GcHeap, form: GcRef) -> GcRef {
        match gc_value!(form) {
            SchemeValue::Vector(_) => strip_datum(heap, form),
            SchemeValue::Pair(head, rest) => {
                let (head, rest) = (*head, *rest);
                if self.names(heap, head, self.quote) {
                    if let SchemeValue::Pair(datum, after) = gc_value!(rest) {
                        let (datum, after) = (*datum, *after);
                        let stripped = strip_datum(heap, datum);
                        if stripped != datum {
                            let tail = new_pair(heap, stripped, after);
                            return new_pair(heap, head, tail);
                        }
                    }
                    return form;
                }
                if self.names(heap, head, self.quasiquote) {
                    let new_rest = self.quasi(heap, rest, 1);
                    return if new_rest == rest { form } else { new_pair(heap, head, new_rest) };
                }
                let new_head = self.code(heap, head);
                let new_rest = self.code(heap, rest);
                if new_head == head && new_rest == rest {
                    form
                } else {
                    new_pair(heap, new_head, new_rest)
                }
            }
            _ => form,
        }
    }

    /// Quasiquoted data at nesting `depth`: identifiers are stripped except
    /// inside an `unquote`/`unquote-splicing` that brings the depth to 0,
    /// which is code again.
    fn quasi(&mut self, heap: &mut GcHeap, form: GcRef, depth: usize) -> GcRef {
        if let Some(done) = self.memo.get(&(form, depth)) {
            return *done;
        }
        self.memo.insert((form, depth), form);
        let result = self.quasi_uncached(heap, form, depth);
        self.memo.insert((form, depth), result);
        result
    }

    fn quasi_uncached(&mut self, heap: &mut GcHeap, form: GcRef, depth: usize) -> GcRef {
        match gc_value!(form) {
            SchemeValue::Symbol(_) => strip(heap, form),
            SchemeValue::Vector(items) => {
                let items = items.clone();
                let new: Vec<GcRef> = items.iter().map(|i| self.quasi(heap, *i, depth)).collect();
                if new == items { form } else { new_vector(heap, new) }
            }
            SchemeValue::Pair(head, rest) => {
                let (head, rest) = (*head, *rest);
                let head_name = symbol_name(strip(heap, head));
                let inner_depth = match head_name {
                    Some("unquote" | "unquote-splicing") if is_identifier(head) => Some(depth - 1),
                    Some("quasiquote") if is_identifier(head) => Some(depth + 1),
                    _ => None,
                };
                let (new_head, new_rest) = match inner_depth {
                    Some(0) => (strip(heap, head), self.code(heap, rest)),
                    Some(d) => (strip(heap, head), self.quasi(heap, rest, d)),
                    None => (self.quasi(heap, head, depth), self.quasi(heap, rest, depth)),
                };
                if new_head == head && new_rest == rest {
                    form
                } else {
                    new_pair(heap, new_head, new_rest)
                }
            }
            _ => form,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::env::Frame;
    use crate::eval::{RunTime, RunTimeStruct, initialize_scheme_globals};
    use std::cell::RefCell;
    use std::rc::Rc;

    /// A runtime with the full global environment (so `quote` and friends
    /// are bound, as the literal-data stripping needs).
    fn setup() -> (RunTimeStruct, EnvRef) {
        let mut ev = RunTimeStruct::new();
        let env = Rc::new(RefCell::new(Frame::new(None)));
        {
            let mut rt = RunTime::from_eval(&mut ev);
            initialize_scheme_globals(&mut rt, env.clone()).unwrap();
        }
        (ev, env)
    }

    fn read(heap: &mut GcHeap, src: &str) -> GcRef {
        let mut port = crate::io::new_string_port_input(src);
        crate::parser::parse(heap, &mut port).unwrap()
    }

    fn rules(heap: &mut GcHeap, env: &EnvRef, src: &str) -> SyntaxRules {
        let form = read(heap, src);
        parse(heap, form, env.clone()).unwrap()
    }

    fn expand(heap: &mut GcHeap, env: &EnvRef, sr: &SyntaxRules, use_src: &str) -> Result<String, String> {
        let form = read(heap, use_src);
        sr.expand(heap, form, env).map(|e| print_value(&e))
    }

    fn parse_err(heap: &mut GcHeap, env: &EnvRef, src: &str) -> String {
        let form = read(heap, src);
        parse(heap, form, env.clone()).err().expect("expected a parse error")
    }

    #[test]
    fn substitutes_pattern_variables_and_renames_the_rest() {
        let (mut ev, env) = setup();
        let heap = &mut ev.heap;
        let sr = rules(heap, &env, "(syntax-rules () ((_ a b) (list b a)))");
        let form = read(heap, "(m 1 2)");
        let e = sr.expand(heap, form, &env).unwrap();
        assert_eq!(print_value(&e), "(list 2 1)");
        let list = crate::gc::car(e).unwrap();
        assert!(heap.alias(list).is_some(), "introduced identifiers are aliases");
        assert_eq!(strip(heap, list), heap.intern_symbol("list"));
    }

    #[test]
    fn one_alias_per_identifier_per_expansion() {
        let (mut ev, env) = setup();
        let heap = &mut ev.heap;
        let sr = rules(heap, &env, "(syntax-rules () ((_ e) (t e t)))");
        let use1 = read(heap, "(m t)");
        let e = sr.expand(heap, use1, &env).unwrap();
        let (items, _) = list_parts(e);
        assert_eq!(items[0], items[2], "both template t's are the same alias");
        assert_eq!(items[1], heap.intern_symbol("t"), "the user's t is untouched");
        assert_ne!(items[0], items[1]);
        let e2 = sr.expand(heap, use1, &env).unwrap();
        assert_ne!(crate::gc::car(e2).unwrap(), items[0], "a new expansion gets new aliases");
    }

    #[test]
    fn ellipsis_in_the_middle_of_a_list() {
        let (mut ev, env) = setup();
        let heap = &mut ev.heap;
        let sr = rules(
            heap,
            &env,
            "(syntax-rules () ((_ a b (m n) ... x y) (v (l a b) (l m ...) (l n ...) (l x y))))",
        );
        assert_eq!(
            expand(heap, &env, &sr, "(p 10 20 (31 32) (41 42) 63 77)").unwrap(),
            "(v (l 10 20) (l 31 41) (l 32 42) (l 63 77))"
        );
        assert_eq!(expand(heap, &env, &sr, "(p 10 20 63 77)").unwrap(), "(v (l 10 20) (l) (l) (l 63 77))");
        assert!(expand(heap, &env, &sr, "(p 10 20 63)").is_err(), "too few elements");
    }

    #[test]
    fn dotted_tails() {
        let (mut ev, env) = setup();
        let heap = &mut ev.heap;
        let sr = rules(heap, &env, "(syntax-rules () ((_ (a x ... . r)) (quote (a (x ...) r))))");
        assert_eq!(expand(heap, &env, &sr, "(p (1 2 3 . 4))").unwrap(), "(quote (1 (2 3) 4))");
        assert_eq!(expand(heap, &env, &sr, "(p (1 2 3))").unwrap(), "(quote (1 (2 3) ()))");
        let sr = rules(heap, &env, "(syntax-rules () ((_ a . rest) (quote rest)))");
        assert_eq!(expand(heap, &env, &sr, "(p 1 2 3)").unwrap(), "(quote (2 3))");
        let sr = rules(heap, &env, "(syntax-rules () ((_ (a b)) 'two))");
        assert!(expand(heap, &env, &sr, "(p 5)").is_err(), "a list pattern doesn't match an atom");
    }

    #[test]
    fn vector_patterns_and_nested_ellipses() {
        let (mut ev, env) = setup();
        let heap = &mut ev.heap;
        let sr = rules(heap, &env, "(syntax-rules () ((_ #(a b ...)) (quote (a b ...))))");
        assert_eq!(expand(heap, &env, &sr, "(p #(1 2 3))").unwrap(), "(quote (1 2 3))");
        let sr = rules(heap, &env, "(syntax-rules () ((_ (a b ...) ...) (quote (b ... ...))))");
        assert_eq!(expand(heap, &env, &sr, "(p (1 2 3) (4 5))").unwrap(), "(quote (2 3 5))");
        let sr = rules(heap, &env, "(syntax-rules () ((_ (a b ...) ...) (quote ((a b ...) ...))))");
        assert_eq!(expand(heap, &env, &sr, "(p (1 2 3) (4 5))").unwrap(), "(quote ((1 2 3) (4 5)))");
    }

    #[test]
    fn literals_match_by_binding_and_underscore_is_a_wildcard() {
        let (mut ev, env) = setup();
        let heap = &mut ev.heap;
        let sr = rules(heap, &env, "(syntax-rules (=>) ((_ a => b) 'arrow) ((_ _ _ _) 'three))");
        assert_eq!(expand(heap, &env, &sr, "(p 1 => 2)").unwrap(), "(quote arrow)");
        assert_eq!(expand(heap, &env, &sr, "(p 1 2 3)").unwrap(), "(quote three)");
        // A use site that binds => sees a variable, not the literal.
        let local = env.extend();
        let arrow = heap.intern_symbol("=>");
        let nil = heap.nil_s();
        local.define(arrow, nil);
        let form = read(heap, "(p 1 => 2)");
        assert_eq!(print_value(&sr.expand(heap, form, &local).unwrap()), "(quote three)");
        // `_` as a literal takes priority over the wildcard.
        let sr = rules(heap, &env, "(syntax-rules (_) ((_ _) 'lit) ((_ x) 'other))");
        assert_eq!(expand(heap, &env, &sr, "(p _)").unwrap(), "(quote lit)");
        assert_eq!(expand(heap, &env, &sr, "(p a)").unwrap(), "(quote other)");
    }

    #[test]
    fn ellipsis_escapes_and_custom_ellipses() {
        let (mut ev, env) = setup();
        let heap = &mut ev.heap;
        let dots = heap.intern_symbol("...");
        let sr = rules(heap, &env, "(syntax-rules () ((_ x) '(... (x ...))))");
        let form = read(heap, "(p 100)");
        let e = sr.expand(heap, form, &env).unwrap();
        assert_eq!(print_value(&e), "(quote (100 ...))");
        let quoted = crate::gc::car(crate::gc::cdr(e).unwrap()).unwrap();
        let (items, _) = list_parts(quoted);
        assert_eq!(items[1], dots, "quoted data holds the plain symbol, not an alias");

        let sr = rules(heap, &env, "(syntax-rules dots () ((_ x dots) '(x dots)))");
        assert_eq!(expand(heap, &env, &sr, "(p 1 2)").unwrap(), "(quote (1 2))");
        // A literal `...` takes priority over `...` as the custom ellipsis.
        let sr = rules(heap, &env, "(syntax-rules ... (...) ((_ x) '(x ...)))");
        assert_eq!(expand(heap, &env, &sr, "(p 100)").unwrap(), "(quote (100 ...))");
    }

    #[test]
    fn quasiquote_strips_data_but_not_unquoted_code() {
        let (mut ev, env) = setup();
        let heap = &mut ev.heap;
        let sr = rules(heap, &env, "(syntax-rules () ((_ e) `(a ,(list e) b)))");
        let form = read(heap, "(p z)");
        let e = sr.expand(heap, form, &env).unwrap();
        assert_eq!(print_value(&e), "(quasiquote (a (unquote (list z)) b))");
        let (top, _) = list_parts(e);
        let (data, _) = list_parts(top[1]);
        assert_eq!(data[0], heap.intern_symbol("a"), "quasiquoted data is stripped");
        let (unq, _) = list_parts(data[1]);
        let (code, _) = list_parts(unq[1]);
        assert!(heap.alias(code[0]).is_some(), "unquoted code keeps its aliases");
    }

    #[test]
    fn no_matching_rule_is_an_error() {
        let (mut ev, env) = setup();
        let heap = &mut ev.heap;
        let sr = rules(heap, &env, "(syntax-rules () ((_ a) a))");
        let err = expand(heap, &env, &sr, "(p 1 2)").unwrap_err();
        assert!(err.contains("no syntax rule matches"), "{}", err);
    }

    #[test]
    fn malformed_rules_are_rejected() {
        let (mut ev, env) = setup();
        let heap = &mut ev.heap;
        assert!(parse_err(heap, &env, "(syntax-rules () ((_ ... x) 1))").contains("must follow"));
        assert!(parse_err(heap, &env, "(syntax-rules () ((_ x ... y ...) 1))").contains("more than one"));
        assert!(parse_err(heap, &env, "(syntax-rules () ((_ x ...) x))").contains("needs"));
        assert!(parse_err(heap, &env, "(syntax-rules () ((_ x x) 1))").contains("twice"));
        assert!(parse_err(heap, &env, "(syntax-rules () (_ 1))").contains("must be a list"));
        assert!(parse_err(heap, &env, "(syntax-rules (1) ((_) 1))").contains("identifiers"));
    }
}
