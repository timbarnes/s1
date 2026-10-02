//! Numbers: R7RS section 6.2.
//!
//! s1 has three representations: exact integers (`SchemeValue::Int`, a
//! `BigInt`), exact non-integer rationals (`SchemeValue::Rational`) and
//! inexact flonums (`SchemeValue::Float`). Exact arithmetic stays exact
//! (`(/ 1 2)` is `1/2`) and whole results come back as integers; any inexact
//! operand makes the result inexact.

use crate::env::{EnvOps, EnvRef};
use crate::gc::{
    GcHeap, GcRef, SchemeValue, new_bool, new_float, new_int, new_rational, new_string, new_values,
};
use crate::gc_value;
use crate::number_syntax::{Number, NumberSyntax, parse_number};
use crate::register_builtin_family;
use num_bigint::BigInt;
use num_integer::Integer;
use num_rational::BigRational;
use num_traits::{One, Signed, ToPrimitive, Zero};
use std::cmp::Ordering;

pub fn register_number_builtins(heap: &mut GcHeap, env: EnvRef) {
    register_builtin_family!(heap, env,
        "+" => (plus_b, "(+ z ...) Returns the sum of its arguments"),
        "-" => (minus_b, "(- z1 z2 ...) Returns z1 minus the rest, or the negation of a single argument"),
        "*" => (times_b, "(* z ...) Returns the product of its arguments"),
        "/" => (div_b, "(/ z1 z2 ...) Returns z1 divided by the rest, or the reciprocal of a single argument; exact division gives an exact rational"),
        "=" => (eq_b, "(= z1 z2 ...) Returns #t if all arguments are numerically equal"),
        "<" => (lt_b, "(< x1 x2 ...) Returns #t if the arguments are strictly increasing"),
        ">" => (gt_b, "(> x1 x2 ...) Returns #t if the arguments are strictly decreasing"),
        "<=" => (le_b, "(<= x1 x2 ...) Returns #t if the arguments are non-decreasing"),
        ">=" => (ge_b, "(>= x1 x2 ...) Returns #t if the arguments are non-increasing"),
        "number?" => (number_q, "(number? obj) Returns #t if obj is a number"),
        "complex?" => (number_q, "(complex? obj) Returns #t if obj is a number (every s1 number is real)"),
        "real?" => (number_q, "(real? obj) Returns #t if obj is a real number"),
        "rational?" => (rational_q, "(rational? obj) Returns #t if obj is an exact number or a finite flonum"),
        "integer?" => (integer_q, "(integer? obj) Returns #t if obj is an integer, exact or inexact (2.0 is an integer)"),
        "exact?" => (exact_q, "(exact? z) Returns #t if z is exact"),
        "inexact?" => (inexact_q, "(inexact? z) Returns #t if z is inexact"),
        "exact-integer?" => (exact_integer_q, "(exact-integer? obj) Returns #t if obj is an exact integer"),
        "nan?" => (nan_q, "(nan? z) Returns #t if z is a NaN"),
        "infinite?" => (infinite_q, "(infinite? z) Returns #t if z is +inf.0 or -inf.0"),
        "finite?" => (finite_q, "(finite? z) Returns #t if z is neither infinite nor a NaN"),
        "zero?" => (zero_q, "(zero? z) Returns #t if z is zero"),
        "positive?" => (positive_q, "(positive? x) Returns #t if x is greater than zero"),
        "negative?" => (negative_q, "(negative? x) Returns #t if x is less than zero"),
        "odd?" => (odd_q, "(odd? n) Returns #t if the integer n is odd"),
        "even?" => (even_q, "(even? n) Returns #t if the integer n is even"),
        "max" => (max_b, "(max x1 x2 ...) Returns the largest argument; inexact if any argument is"),
        "min" => (min_b, "(min x1 x2 ...) Returns the smallest argument; inexact if any argument is"),
        "abs" => (abs_b, "(abs x) Returns the absolute value of x"),
        "quotient" => (truncate_quotient_b, "(quotient n1 n2) Integer division rounding toward zero"),
        "remainder" => (truncate_remainder_b, "(remainder n1 n2) Remainder of (quotient n1 n2); has the sign of n1"),
        "modulo" => (floor_remainder_b, "(modulo n1 n2) Remainder of floor division; has the sign of n2"),
        "floor/" => (floor_div_b, "(floor/ n1 n2) Returns two values: the floor quotient and remainder"),
        "floor-quotient" => (floor_quotient_b, "(floor-quotient n1 n2) Integer division rounding toward negative infinity"),
        "floor-remainder" => (floor_remainder_b, "(floor-remainder n1 n2) Remainder of floor division; has the sign of n2"),
        "truncate/" => (truncate_div_b, "(truncate/ n1 n2) Returns two values: the truncated quotient and remainder"),
        "truncate-quotient" => (truncate_quotient_b, "(truncate-quotient n1 n2) Integer division rounding toward zero"),
        "truncate-remainder" => (truncate_remainder_b, "(truncate-remainder n1 n2) Remainder of truncated division; has the sign of n1"),
        "gcd" => (gcd_b, "(gcd n ...) Returns the greatest common divisor of its arguments (0 for none)"),
        "lcm" => (lcm_b, "(lcm n ...) Returns the least common multiple of its arguments (1 for none)"),
        "numerator" => (numerator_b, "(numerator q) Returns the numerator of q in lowest terms"),
        "denominator" => (denominator_b, "(denominator q) Returns the denominator of q in lowest terms"),
        "floor" => (floor_b, "(floor x) Returns the largest integer not greater than x"),
        "ceiling" => (ceiling_b, "(ceiling x) Returns the smallest integer not less than x"),
        "truncate" => (truncate_b, "(truncate x) Returns the integer nearest x whose magnitude is not larger"),
        "round" => (round_b, "(round x) Returns the integer nearest x, rounding halves to even"),
        "rationalize" => (rationalize_b, "(rationalize x y) Returns the simplest rational differing from x by no more than y"),
        "exp" => (exp_b, "(exp z) Returns e raised to the power z"),
        "log" => (log_b, "(log z [base]) Returns the natural logarithm of z, or its logarithm in base"),
        "sin" => (sin_b, "(sin z) Returns the sine of z"),
        "cos" => (cos_b, "(cos z) Returns the cosine of z"),
        "tan" => (tan_b, "(tan z) Returns the tangent of z"),
        "asin" => (asin_b, "(asin z) Returns the arcsine of z"),
        "acos" => (acos_b, "(acos z) Returns the arccosine of z"),
        "atan" => (atan_b, "(atan z) or (atan y x) Returns the arctangent of z, or of y/x using the signs of both"),
        "square" => (square_b, "(square z) Returns z times z"),
        "sqrt" => (sqrt_b, "(sqrt z) Returns the square root of z; exact when z is an exact perfect square"),
        "exact-integer-sqrt" => (exact_integer_sqrt_b, "(exact-integer-sqrt k) Returns two values s and r with k = s*s + r"),
        "expt" => (expt_b, "(expt z1 z2) Returns z1 raised to the power z2; exact for an exact base and integer exponent"),
        "exact" => (exact_b, "(exact z) Returns the exact number closest to z"),
        "inexact" => (inexact_b, "(inexact z) Returns the inexact number closest to z"),
        "exact->inexact" => (inexact_b, "(exact->inexact z) Same as inexact"),
        "inexact->exact" => (exact_b, "(inexact->exact z) Same as exact"),
        "number->string" => (number_to_string_b, "(number->string z [radix]) Returns the external representation of z in radix 2, 8, 10 (the default) or 16"),
        "string->number" => (string_to_number_b, "(string->number string [radix]) Returns the number string represents, or #f"),
    );
}

// ---------------------------------------------------------------------------
// The working representation
// ---------------------------------------------------------------------------

/// A number taken out of the heap for computation.
#[derive(Clone, Debug, PartialEq)]
enum Num {
    Int(BigInt),
    /// Always a non-integer: see `Num::from_ratio`.
    Rat(BigRational),
    Float(f64),
}

impl Num {
    /// The number `v` holds, or an error naming `who`.
    fn of(v: GcRef, who: &str) -> Result<Num, String> {
        match gc_value!(v) {
            SchemeValue::Int(i) => Ok(Num::Int(i.clone())),
            SchemeValue::Rational(r) => Ok(Num::Rat((**r).clone())),
            SchemeValue::Float(f) => Ok(Num::Float(*f)),
            _ => Err(format!(
                "{}: expected a number, got {}",
                who,
                crate::printer::print_value(&v)
            )),
        }
    }

    /// The exact number `r`, as an `Int` when it is whole.
    fn from_ratio(r: BigRational) -> Num {
        if r.is_integer() {
            Num::Int(r.to_integer())
        } else {
            Num::Rat(r)
        }
    }

    fn alloc(self, heap: &mut GcHeap) -> GcRef {
        match self {
            Num::Int(i) => new_int(heap, i),
            Num::Rat(r) => new_rational(heap, r),
            Num::Float(f) => new_float(heap, f),
        }
    }

    fn is_exact(&self) -> bool {
        !matches!(self, Num::Float(_))
    }

    fn to_f64(&self) -> f64 {
        match self {
            Num::Int(i) => int_to_f64(i),
            Num::Rat(r) => r.to_f64().unwrap_or(f64::NAN),
            Num::Float(f) => *f,
        }
    }

    /// The exact value of an exact number. Flonums go through
    /// `float_to_exact`; this is only for operands already known exact.
    fn to_ratio(&self) -> BigRational {
        match self {
            Num::Int(i) => BigRational::from_integer(i.clone()),
            Num::Rat(r) => r.clone(),
            Num::Float(f) => BigRational::from_float(*f).unwrap_or_else(BigRational::zero),
        }
    }

    fn to_inexact(self) -> Num {
        match self {
            Num::Float(_) => self,
            n => Num::Float(n.to_f64()),
        }
    }

    fn to_exact(self, who: &str) -> Result<Num, String> {
        match self {
            Num::Float(f) => float_to_exact(f, who),
            n => Ok(n),
        }
    }

    fn sign(&self) -> Ordering {
        match self {
            Num::Int(i) => i.sign().cmp(&num_bigint::Sign::NoSign),
            Num::Rat(r) => r.numer().sign().cmp(&num_bigint::Sign::NoSign),
            Num::Float(f) => f.partial_cmp(&0.0).unwrap_or(Ordering::Equal),
        }
    }

    /// The integer this number is, exact or inexact, if it is one.
    fn as_integer(&self) -> Option<BigInt> {
        match self {
            Num::Int(i) => Some(i.clone()),
            Num::Float(f) if f.is_finite() && f.fract() == 0.0 => float_to_bigint(*f),
            _ => None,
        }
    }
}

fn int_to_f64(i: &BigInt) -> f64 {
    i.to_f64().unwrap_or(if i.is_negative() {
        f64::NEG_INFINITY
    } else {
        f64::INFINITY
    })
}

fn float_to_bigint(f: f64) -> Option<BigInt> {
    BigRational::from_float(f).map(|r| r.to_integer())
}

/// The exact value of a flonum (`(exact 0.5)` is `1/2`).
fn float_to_exact(f: f64, who: &str) -> Result<Num, String> {
    BigRational::from_float(f)
        .map(Num::from_ratio)
        .ok_or_else(|| format!("{}: {} has no exact representation", who, crate::printer::format_float(f)))
}

fn nums(args: &[GcRef], who: &str) -> Result<Vec<Num>, String> {
    args.iter().map(|a| Num::of(*a, who)).collect()
}

fn arity(args: &[GcRef], n: usize, who: &str) -> Result<(), String> {
    if args.len() == n {
        Ok(())
    } else {
        Err(format!(
            "{}: expects {} argument{}, got {}",
            who,
            n,
            if n == 1 { "" } else { "s" },
            args.len()
        ))
    }
}

// ---------------------------------------------------------------------------
// Arithmetic
// ---------------------------------------------------------------------------

#[derive(Clone, Copy)]
enum Op {
    Add,
    Sub,
    Mul,
}

/// `acc op x`, staying exact when both are exact. The integer case, by far
/// the most common, works in place on the accumulator.
fn combine(acc: Num, x: &SchemeValue, op: Op) -> Num {
    match (acc, x) {
        (Num::Int(mut a), SchemeValue::Int(b)) => {
            match op {
                Op::Add => a += b,
                Op::Sub => a -= b,
                Op::Mul => a *= b,
            }
            Num::Int(a)
        }
        (Num::Float(a), x) => Num::Float(float_op(a, value_to_f64(x), op)),
        (acc, SchemeValue::Float(b)) => Num::Float(float_op(acc.to_f64(), *b, op)),
        (acc, x) => {
            let a = acc.to_ratio();
            let b = match x {
                SchemeValue::Int(i) => BigRational::from_integer(i.clone()),
                SchemeValue::Rational(r) => (**r).clone(),
                _ => unreachable!("combine: checked number"),
            };
            Num::from_ratio(match op {
                Op::Add => a + b,
                Op::Sub => a - b,
                Op::Mul => a * b,
            })
        }
    }
}

fn float_op(a: f64, b: f64, op: Op) -> f64 {
    match op {
        Op::Add => a + b,
        Op::Sub => a - b,
        Op::Mul => a * b,
    }
}

fn value_to_f64(v: &SchemeValue) -> f64 {
    match v {
        SchemeValue::Int(i) => int_to_f64(i),
        SchemeValue::Rational(r) => r.to_f64().unwrap_or(f64::NAN),
        SchemeValue::Float(f) => *f,
        _ => f64::NAN,
    }
}

fn check_numbers(args: &[GcRef], who: &str) -> Result<(), String> {
    for a in args {
        if !matches!(
            gc_value!(*a),
            SchemeValue::Int(_) | SchemeValue::Rational(_) | SchemeValue::Float(_)
        ) {
            return Err(format!(
                "{}: expected a number, got {}",
                who,
                crate::printer::print_value(a)
            ));
        }
    }
    Ok(())
}

fn fold(heap: &mut GcHeap, args: &[GcRef], init: Num, op: Op, who: &str) -> Result<GcRef, String> {
    check_numbers(args, who)?;
    let mut acc = init;
    for a in args {
        acc = combine(acc, gc_value!(*a), op);
    }
    Ok(acc.alloc(heap))
}

pub fn plus_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        check_numbers(args, "+")?;
        return Ok(args[0]);
    }
    fold(heap, args, Num::Int(BigInt::zero()), Op::Add, "+")
}

pub fn times_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        check_numbers(args, "*")?;
        return Ok(args[0]);
    }
    fold(heap, args, Num::Int(BigInt::one()), Op::Mul, "*")
}

pub fn minus_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    match args {
        [] => Err("-: expects at least 1 argument".to_string()),
        [x] => Ok(match Num::of(*x, "-")? {
            Num::Int(i) => Num::Int(-i),
            Num::Rat(r) => Num::Rat(-r),
            Num::Float(f) => Num::Float(-f),
        }
        .alloc(heap)),
        [first, rest @ ..] => {
            let init = Num::of(*first, "-")?;
            fold(heap, rest, init, Op::Sub, "-")
        }
    }
}

/// `a / b`. Exact division by exact zero is an error; inexact division
/// follows IEEE (`(/ 1.0 0)` is `+inf.0`).
fn divide(a: Num, b: Num) -> Result<Num, String> {
    if a.is_exact() && b.is_exact() {
        if b.sign() == Ordering::Equal {
            return Err("/: division by zero".to_string());
        }
        if let (Num::Int(x), Num::Int(y)) = (&a, &b) {
            if (x % y).is_zero() {
                return Ok(Num::Int(x / y));
            }
        }
        Ok(Num::from_ratio(a.to_ratio() / b.to_ratio()))
    } else {
        Ok(Num::Float(a.to_f64() / b.to_f64()))
    }
}

pub fn div_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let ns = nums(args, "/")?;
    let mut iter = ns.into_iter();
    let first = iter.next().ok_or("/: expects at least 1 argument")?;
    let mut acc = if args.len() == 1 {
        divide(Num::Int(BigInt::one()), first)?
    } else {
        first
    };
    for n in iter {
        acc = divide(acc, n)?;
    }
    Ok(acc.alloc(heap))
}

pub fn abs_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, "abs")?;
    Ok(match Num::of(args[0], "abs")? {
        Num::Int(i) if i.is_negative() => Num::Int(-i).alloc(heap),
        Num::Rat(r) if r.is_negative() => Num::Rat(-r).alloc(heap),
        Num::Float(f) => new_float(heap, f.abs()),
        _ => args[0],
    })
}

pub fn square_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, "square")?;
    let n = Num::of(args[0], "square")?;
    Ok(combine(n, gc_value!(args[0]), Op::Mul).alloc(heap))
}

// ---------------------------------------------------------------------------
// Comparison
// ---------------------------------------------------------------------------

/// Compare two numbers exactly. A finite flonum is compared by its exact
/// value, so the comparison is transitive even across magnitudes where
/// converting the exact side to `f64` would round (`(< 9007199254740993
/// 9007199254740992.0)` is `#f`). Returns `None` when a NaN is involved.
fn num_cmp(a: &SchemeValue, b: &SchemeValue) -> Option<Ordering> {
    use SchemeValue::{Float, Int, Rational};
    match (a, b) {
        (Int(x), Int(y)) => Some(x.cmp(y)),
        (Float(x), Float(y)) => x.partial_cmp(y),
        (Int(x), Float(y)) if x.bits() <= 53 => int_to_f64(x).partial_cmp(y),
        (Float(x), Int(y)) if y.bits() <= 53 => x.partial_cmp(&int_to_f64(y)),
        (Float(x), _) => exact_vs_float(b, *x).map(Ordering::reverse),
        (_, Float(y)) => exact_vs_float(a, *y),
        (Rational(x), Rational(y)) => Some(x.cmp(y)),
        (Int(x), Rational(y)) => Some(BigRational::from_integer(x.clone()).cmp(y)),
        (Rational(x), Int(y)) => Some((**x).cmp(&BigRational::from_integer(y.clone()))),
        _ => None,
    }
}

/// Order an exact number against a flonum.
fn exact_vs_float(exact: &SchemeValue, f: f64) -> Option<Ordering> {
    if f.is_nan() {
        return None;
    }
    if f.is_infinite() {
        return Some(if f > 0.0 { Ordering::Less } else { Ordering::Greater });
    }
    let fr = BigRational::from_float(f)?;
    Some(match exact {
        SchemeValue::Int(i) => BigRational::from_integer(i.clone()).cmp(&fr),
        SchemeValue::Rational(r) => (**r).cmp(&fr),
        _ => return None,
    })
}

fn compare_chain(
    heap: &mut GcHeap,
    args: &[GcRef],
    name: &str,
    ok: fn(Ordering) -> bool,
) -> Result<GcRef, String> {
    if args.len() < 2 {
        return Err(format!("{}: expects at least 2 arguments", name));
    }
    check_numbers(args, name)?;
    let result = args
        .windows(2)
        .all(|pair| num_cmp(gc_value!(pair[0]), gc_value!(pair[1])).is_some_and(ok));
    Ok(new_bool(heap, result))
}

pub fn eq_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare_chain(heap, args, "=", Ordering::is_eq)
}

pub fn lt_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare_chain(heap, args, "<", Ordering::is_lt)
}

pub fn gt_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare_chain(heap, args, ">", Ordering::is_gt)
}

pub fn le_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare_chain(heap, args, "<=", Ordering::is_le)
}

pub fn ge_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare_chain(heap, args, ">=", Ordering::is_ge)
}

/// `max` / `min`: the extreme argument, made inexact if any argument is.
fn extreme(heap: &mut GcHeap, args: &[GcRef], who: &str, want: Ordering) -> Result<GcRef, String> {
    if args.is_empty() {
        return Err(format!("{}: expects at least 1 argument", who));
    }
    check_numbers(args, who)?;
    let mut best = args[0];
    let mut inexact = false;
    for a in args {
        match gc_value!(*a) {
            SchemeValue::Float(f) => {
                inexact = true;
                if f.is_nan() {
                    return Ok(*a);
                }
            }
            _ => {}
        }
        if num_cmp(gc_value!(*a), gc_value!(best)) == Some(want) {
            best = *a;
        }
    }
    if inexact && !matches!(gc_value!(best), SchemeValue::Float(_)) {
        Ok(new_float(heap, value_to_f64(gc_value!(best))))
    } else {
        Ok(best)
    }
}

pub fn max_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    extreme(heap, args, "max", Ordering::Greater)
}

pub fn min_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    extreme(heap, args, "min", Ordering::Less)
}

// ---------------------------------------------------------------------------
// Predicates
// ---------------------------------------------------------------------------

fn bool_of(heap: &mut GcHeap, args: &[GcRef], who: &str, test: fn(&SchemeValue) -> bool) -> Result<GcRef, String> {
    arity(args, 1, who)?;
    let result = test(gc_value!(args[0]));
    Ok(new_bool(heap, result))
}

/// Like `bool_of`, for predicates whose argument must be a number.
fn num_pred(heap: &mut GcHeap, args: &[GcRef], who: &str, test: fn(&Num) -> bool) -> Result<GcRef, String> {
    arity(args, 1, who)?;
    let n = Num::of(args[0], who)?;
    Ok(new_bool(heap, test(&n)))
}

pub fn number_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    bool_of(heap, args, "number?", |v| {
        matches!(v, SchemeValue::Int(_) | SchemeValue::Rational(_) | SchemeValue::Float(_))
    })
}

pub fn rational_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    bool_of(heap, args, "rational?", |v| match v {
        SchemeValue::Int(_) | SchemeValue::Rational(_) => true,
        SchemeValue::Float(f) => f.is_finite(),
        _ => false,
    })
}

pub fn integer_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    bool_of(heap, args, "integer?", |v| match v {
        SchemeValue::Int(_) => true,
        SchemeValue::Float(f) => f.is_finite() && f.fract() == 0.0,
        _ => false,
    })
}

pub fn exact_integer_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    bool_of(heap, args, "exact-integer?", |v| matches!(v, SchemeValue::Int(_)))
}

pub fn exact_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    num_pred(heap, args, "exact?", Num::is_exact)
}

pub fn inexact_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    num_pred(heap, args, "inexact?", |n| !n.is_exact())
}

pub fn nan_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    num_pred(heap, args, "nan?", |n| matches!(n, Num::Float(f) if f.is_nan()))
}

pub fn infinite_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    num_pred(heap, args, "infinite?", |n| matches!(n, Num::Float(f) if f.is_infinite()))
}

pub fn finite_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    num_pred(heap, args, "finite?", |n| match n {
        Num::Float(f) => f.is_finite(),
        _ => true,
    })
}

pub fn zero_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    num_pred(heap, args, "zero?", |n| match n {
        Num::Float(f) => *f == 0.0,
        n => n.sign() == Ordering::Equal,
    })
}

pub fn positive_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    num_pred(heap, args, "positive?", |n| match n {
        Num::Float(f) => *f > 0.0,
        n => n.sign() == Ordering::Greater,
    })
}

pub fn negative_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    num_pred(heap, args, "negative?", |n| match n {
        Num::Float(f) => *f < 0.0,
        n => n.sign() == Ordering::Less,
    })
}

fn parity(heap: &mut GcHeap, args: &[GcRef], who: &str, want_even: bool) -> Result<GcRef, String> {
    arity(args, 1, who)?;
    let n = Num::of(args[0], who)?
        .as_integer()
        .ok_or_else(|| format!("{}: expected an integer", who))?;
    Ok(new_bool(heap, n.is_even() == want_even))
}

pub fn odd_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    parity(heap, args, "odd?", false)
}

pub fn even_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    parity(heap, args, "even?", true)
}

// ---------------------------------------------------------------------------
// Integer division
// ---------------------------------------------------------------------------

/// The two integer operands of a division procedure, and whether the result
/// must be inexact (an operand was an integral flonum).
fn int_pair(args: &[GcRef], who: &str) -> Result<(BigInt, BigInt, bool), String> {
    arity(args, 2, who)?;
    let a = Num::of(args[0], who)?;
    let b = Num::of(args[1], who)?;
    let inexact = !a.is_exact() || !b.is_exact();
    let to_int = |n: &Num| n.as_integer().ok_or_else(|| format!("{}: expected integers", who));
    let (x, y) = (to_int(&a)?, to_int(&b)?);
    if y.is_zero() {
        return Err(format!("{}: division by zero", who));
    }
    Ok((x, y, inexact))
}

fn int_result(heap: &mut GcHeap, n: BigInt, inexact: bool) -> GcRef {
    if inexact {
        new_float(heap, int_to_f64(&n))
    } else {
        new_int(heap, n)
    }
}

pub fn floor_quotient_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let (a, b, inexact) = int_pair(args, "floor-quotient")?;
    Ok(int_result(heap, a.div_floor(&b), inexact))
}

pub fn floor_remainder_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let (a, b, inexact) = int_pair(args, "floor-remainder")?;
    Ok(int_result(heap, a.mod_floor(&b), inexact))
}

pub fn floor_div_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let (a, b, inexact) = int_pair(args, "floor/")?;
    let (q, r) = a.div_mod_floor(&b);
    let vals = vec![int_result(heap, q, inexact), int_result(heap, r, inexact)];
    Ok(new_values(heap, vals))
}

pub fn truncate_quotient_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let (a, b, inexact) = int_pair(args, "truncate-quotient")?;
    Ok(int_result(heap, a / b, inexact))
}

pub fn truncate_remainder_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let (a, b, inexact) = int_pair(args, "truncate-remainder")?;
    Ok(int_result(heap, a % b, inexact))
}

pub fn truncate_div_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let (a, b, inexact) = int_pair(args, "truncate/")?;
    let (q, r) = a.div_rem(&b);
    let vals = vec![int_result(heap, q, inexact), int_result(heap, r, inexact)];
    Ok(new_values(heap, vals))
}

fn gcd_lcm(heap: &mut GcHeap, args: &[GcRef], who: &str, lcm: bool) -> Result<GcRef, String> {
    let mut acc = if lcm { BigInt::one() } else { BigInt::zero() };
    let mut inexact = false;
    for a in args {
        let n = Num::of(*a, who)?;
        inexact |= !n.is_exact();
        let i = n.as_integer().ok_or_else(|| format!("{}: expected integers", who))?;
        acc = if lcm { acc.lcm(&i) } else { acc.gcd(&i) };
    }
    Ok(int_result(heap, acc.abs(), inexact))
}

pub fn gcd_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    gcd_lcm(heap, args, "gcd", false)
}

pub fn lcm_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    gcd_lcm(heap, args, "lcm", true)
}

// ---------------------------------------------------------------------------
// Rationals and rounding
// ---------------------------------------------------------------------------

/// `numerator` / `denominator`. For a flonum these are of its exact value,
/// returned inexact: `(denominator 0.5)` is `2.0`.
fn ratio_part(heap: &mut GcHeap, args: &[GcRef], who: &str, numer: bool) -> Result<GcRef, String> {
    arity(args, 1, who)?;
    let n = Num::of(args[0], who)?;
    let inexact = !n.is_exact();
    let r = n.to_exact(who)?.to_ratio();
    let part = if numer { r.numer().clone() } else { r.denom().clone() };
    Ok(int_result(heap, part, inexact))
}

pub fn numerator_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    ratio_part(heap, args, "numerator", true)
}

pub fn denominator_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    ratio_part(heap, args, "denominator", false)
}

/// Round an exact rational to an integer, halves to even.
fn round_half_even(r: &BigRational) -> BigInt {
    let floor = r.floor();
    let diff = r - &floor;
    let half = BigRational::new(BigInt::one(), BigInt::from(2));
    let floor = floor.to_integer();
    match diff.cmp(&half) {
        Ordering::Less => floor,
        Ordering::Greater => floor + 1,
        Ordering::Equal if floor.is_even() => floor,
        Ordering::Equal => floor + 1,
    }
}

/// Shared shape of `floor`, `ceiling`, `truncate` and `round`: integers are
/// returned as they are, rationals become exact integers, flonums stay
/// flonums.
fn rounding(
    heap: &mut GcHeap,
    args: &[GcRef],
    who: &str,
    exact: fn(&BigRational) -> BigInt,
    inexact: fn(f64) -> f64,
) -> Result<GcRef, String> {
    arity(args, 1, who)?;
    Ok(match Num::of(args[0], who)? {
        Num::Int(_) => args[0],
        Num::Rat(r) => new_int(heap, exact(&r)),
        Num::Float(f) => new_float(heap, inexact(f)),
    })
}

pub fn floor_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    rounding(heap, args, "floor", |r| r.floor().to_integer(), f64::floor)
}

pub fn ceiling_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    rounding(heap, args, "ceiling", |r| r.ceil().to_integer(), f64::ceil)
}

pub fn truncate_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    rounding(heap, args, "truncate", |r| r.trunc().to_integer(), f64::trunc)
}

pub fn round_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    rounding(heap, args, "round", round_half_even, f64::round_ties_even)
}

/// The simplest rational in the closed interval [lo, hi] (lo <= hi): the
/// one with the smallest denominator, and among those the smallest
/// numerator.
fn simplest_between(lo: &BigRational, hi: &BigRational) -> BigRational {
    if !lo.is_positive() && !hi.is_negative() {
        BigRational::zero()
    } else if hi.is_negative() {
        -simplest_between(&-hi, &-lo)
    } else {
        simplest_positive(lo, hi)
    }
}

/// `simplest_between` for 0 < lo <= hi.
fn simplest_positive(lo: &BigRational, hi: &BigRational) -> BigRational {
    let fl = lo.floor();
    if &fl == lo {
        return fl;
    }
    let next = &fl + BigRational::one();
    if &next <= hi {
        return next;
    }
    // lo and hi lie strictly inside (fl, fl + 1): recurse on the
    // reciprocals of their fractional parts.
    let inner = simplest_positive(&(hi - &fl).recip(), &(lo - &fl).recip());
    fl + inner.recip()
}

pub fn rationalize_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 2, "rationalize")?;
    let x = Num::of(args[0], "rationalize")?;
    let y = Num::of(args[1], "rationalize")?;
    let inexact = !x.is_exact() || !y.is_exact();
    let (xf, yf) = (x.to_f64(), y.to_f64());
    if inexact && (!xf.is_finite() || !yf.is_finite()) {
        let result = if xf.is_nan() || yf.is_nan() || (xf.is_infinite() && yf.is_infinite()) {
            f64::NAN
        } else if yf.is_infinite() {
            0.0
        } else {
            xf
        };
        return Ok(new_float(heap, result));
    }
    let x = x.to_exact("rationalize")?.to_ratio();
    let y = y.to_exact("rationalize")?.to_ratio().abs();
    let r = Num::from_ratio(simplest_between(&(&x - &y), &(&x + &y)));
    Ok(if inexact { r.to_inexact() } else { r }.alloc(heap))
}

// ---------------------------------------------------------------------------
// Transcendental functions, roots and powers
// ---------------------------------------------------------------------------

fn float_fn(heap: &mut GcHeap, args: &[GcRef], who: &str, f: fn(f64) -> f64) -> Result<GcRef, String> {
    arity(args, 1, who)?;
    let x = Num::of(args[0], who)?.to_f64();
    Ok(new_float(heap, f(x)))
}

pub fn exp_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    float_fn(heap, args, "exp", f64::exp)
}

pub fn sin_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    float_fn(heap, args, "sin", f64::sin)
}

pub fn cos_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    float_fn(heap, args, "cos", f64::cos)
}

pub fn tan_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    float_fn(heap, args, "tan", f64::tan)
}

pub fn asin_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    float_fn(heap, args, "asin", f64::asin)
}

pub fn acos_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    float_fn(heap, args, "acos", f64::acos)
}

pub fn log_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    match args.len() {
        1 => float_fn(heap, args, "log", f64::ln),
        2 => {
            let z = Num::of(args[0], "log")?.to_f64();
            let base = Num::of(args[1], "log")?.to_f64();
            Ok(new_float(heap, z.ln() / base.ln()))
        }
        n => Err(format!("log: expects 1 or 2 arguments, got {}", n)),
    }
}

pub fn atan_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    match args.len() {
        1 => float_fn(heap, args, "atan", f64::atan),
        2 => {
            let y = Num::of(args[0], "atan")?.to_f64();
            let x = Num::of(args[1], "atan")?.to_f64();
            Ok(new_float(heap, y.atan2(x)))
        }
        n => Err(format!("atan: expects 1 or 2 arguments, got {}", n)),
    }
}

/// The exact square root of a non-negative integer, if it has one.
fn exact_root(n: &BigInt) -> Option<BigInt> {
    let s = n.sqrt();
    (&s * &s == *n).then_some(s)
}

pub fn sqrt_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, "sqrt")?;
    let n = Num::of(args[0], "sqrt")?;
    if n.sign() == Ordering::Less {
        return Err("sqrt: complex results are not supported".to_string());
    }
    let exact = match &n {
        Num::Int(i) => exact_root(i).map(Num::Int),
        Num::Rat(r) => match (exact_root(r.numer()), exact_root(r.denom())) {
            (Some(a), Some(b)) => Some(Num::from_ratio(BigRational::new(a, b))),
            _ => None,
        },
        Num::Float(_) => None,
    };
    Ok(exact.unwrap_or_else(|| Num::Float(n.to_f64().sqrt())).alloc(heap))
}

pub fn exact_integer_sqrt_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, "exact-integer-sqrt")?;
    match gc_value!(args[0]) {
        SchemeValue::Int(k) if !k.is_negative() => {
            let s = k.sqrt();
            let r = k - &s * &s;
            let vals = vec![new_int(heap, s), new_int(heap, r)];
            Ok(new_values(heap, vals))
        }
        _ => Err("exact-integer-sqrt: expected a non-negative exact integer".to_string()),
    }
}

/// Exponents above this are refused for exact powers, which would otherwise
/// try to build an astronomically large integer.
const MAX_EXACT_POWER: u32 = 1 << 24;

pub fn expt_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 2, "expt")?;
    let base = Num::of(args[0], "expt")?;
    let exp = Num::of(args[1], "expt")?;
    if let Num::Int(e) = &exp {
        if base.is_exact() {
            return Ok(exact_power(base, e)?.alloc(heap));
        }
        if let Some(e) = e.to_i32() {
            return Ok(new_float(heap, base.to_f64().powi(e)));
        }
    }
    let (b, e) = (base.to_f64(), exp.to_f64());
    let result = b.powf(e);
    if result.is_nan() && !b.is_nan() && !e.is_nan() {
        return Err("expt: complex results are not supported".to_string());
    }
    Ok(new_float(heap, result))
}

fn exact_power(base: Num, e: &BigInt) -> Result<Num, String> {
    let r = base.to_ratio();
    // 0, 1 and -1 stay small whatever the exponent.
    if r.is_zero() {
        return match e.sign() {
            num_bigint::Sign::Minus => Err("expt: division by zero".to_string()),
            num_bigint::Sign::NoSign => Ok(Num::Int(BigInt::one())),
            num_bigint::Sign::Plus => Ok(Num::Int(BigInt::zero())),
        };
    }
    if r.abs().is_one() {
        let odd = e.is_odd();
        return Ok(Num::Int(if r.is_negative() && odd { -BigInt::one() } else { BigInt::one() }));
    }
    let mag = e
        .abs()
        .to_u32()
        .filter(|m| *m <= MAX_EXACT_POWER)
        .ok_or("expt: exponent too large")?;
    let p = num_traits::pow(r, mag as usize);
    Ok(Num::from_ratio(if e.is_negative() { p.recip() } else { p }))
}

pub fn exact_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, "exact")?;
    match Num::of(args[0], "exact")? {
        Num::Float(f) => Ok(float_to_exact(f, "exact")?.alloc(heap)),
        _ => Ok(args[0]),
    }
}

pub fn inexact_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, "inexact")?;
    match Num::of(args[0], "inexact")? {
        Num::Float(_) => Ok(args[0]),
        n => Ok(n.to_inexact().alloc(heap)),
    }
}

// ---------------------------------------------------------------------------
// Conversion to and from strings
// ---------------------------------------------------------------------------

fn radix_arg(args: &[GcRef], index: usize, who: &str) -> Result<u32, String> {
    match args.get(index).map(|r| gc_value!(*r)) {
        None => Ok(10),
        Some(SchemeValue::Int(r)) => match r.to_u32() {
            Some(r @ (2 | 8 | 10 | 16)) => Ok(r),
            _ => Err(format!("{}: radix must be 2, 8, 10 or 16", who)),
        },
        Some(_) => Err(format!("{}: radix must be an integer", who)),
    }
}

/// (number->string z [radix])
pub fn number_to_string_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.is_empty() || args.len() > 2 {
        return Err("number->string: expects 1 or 2 arguments".to_string());
    }
    let radix = radix_arg(args, 1, "number->string")?;
    let s = match Num::of(args[0], "number->string")? {
        Num::Int(i) => i.to_str_radix(radix),
        Num::Rat(r) => format!("{}/{}", r.numer().to_str_radix(radix), r.denom().to_str_radix(radix)),
        Num::Float(f) if radix == 10 => crate::printer::format_float(f),
        Num::Float(_) => {
            return Err("number->string: inexact numbers support only radix 10".to_string());
        }
    };
    Ok(new_string(heap, &s))
}

/// (string->number string [radix])
pub fn string_to_number_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.is_empty() || args.len() > 2 {
        return Err("string->number: expects 1 or 2 arguments".to_string());
    }
    let radix = radix_arg(args, 1, "string->number")?;
    let text = match gc_value!(args[0]) {
        SchemeValue::Str(s) => s.clone(),
        _ => return Err("string->number: expected a string".to_string()),
    };
    Ok(match parse_number(&text, radix) {
        NumberSyntax::Value(Number::Int(i)) => new_int(heap, i),
        NumberSyntax::Value(Number::Rational(r)) => new_rational(heap, r),
        NumberSyntax::Value(Number::Float(f)) => new_float(heap, f),
        NumberSyntax::NotANumber | NumberSyntax::Error(_) => new_bool(heap, false),
    })
}

#[cfg(test)]
mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_plus_builtin() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);
        let args = vec![
            new_int(ec.heap, BigInt::from(1)),
            new_int(ec.heap, BigInt::from(2)),
            new_int(ec.heap, BigInt::from(3)),
        ];
        let result = plus_b(&mut ec.heap, &args).unwrap();
        match &gc_value!(result) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "6"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_minus_builtin() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);
        let args = vec![
            new_int(ec.heap, BigInt::from(10)),
            new_int(ec.heap, BigInt::from(3)),
        ];
        let result = minus_b(&mut ec.heap, &args).unwrap();
        match &gc_value!(result) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "7"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_times_builtin() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);
        let args = vec![
            new_int(ec.heap, BigInt::from(2)),
            new_int(ec.heap, BigInt::from(3)),
            new_int(ec.heap, BigInt::from(4)),
        ];
        let result = times_b(&mut ec.heap, &args).unwrap();
        match &gc_value!(result) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "24"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_div_builtin() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);
        let args = vec![
            new_int(ec.heap, BigInt::from(10)),
            new_int(ec.heap, BigInt::from(2)),
        ];
        let result = div_b(&mut ec.heap, &args).unwrap();
        // Exact division stays exact.
        match &gc_value!(result) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "5"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_mod_builtin() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);
        let args = vec![
            new_int(ec.heap, BigInt::from(7)),
            new_int(ec.heap, BigInt::from(3)),
        ];
        let result = floor_remainder_b(&mut ec.heap, &args).unwrap();
        match &gc_value!(result) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "1"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_eq_builtin() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);

        // Test equal integers
        let args = vec![
            new_int(ec.heap, BigInt::from(5)),
            new_int(ec.heap, BigInt::from(5)),
        ];
        let result = eq_b(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(true)));

        // Test unequal integers
        let args = vec![
            new_int(ec.heap, BigInt::from(5)),
            new_int(ec.heap, BigInt::from(6)),
        ];
        let result = eq_b(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(false)));

        // Test mixed int and float
        let args = vec![new_int(ec.heap, BigInt::from(5)), new_float(ec.heap, 5.0)];
        let result = eq_b(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(true)));

        // Test multiple equal values
        let args = vec![
            new_int(ec.heap, BigInt::from(5)),
            new_float(ec.heap, 5.0),
            new_int(ec.heap, BigInt::from(5)),
        ];
        let result = eq_b(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(true)));

        // Test multiple unequal values
        let args = vec![
            new_int(ec.heap, BigInt::from(5)),
            new_float(ec.heap, 5.0),
            new_int(ec.heap, BigInt::from(6)),
        ];
        let result = eq_b(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(false)));
    }

    #[test]
    fn test_lt_builtin() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);

        // Test strictly increasing integers
        let args = vec![
            new_int(ec.heap, BigInt::from(1)),
            new_int(ec.heap, BigInt::from(2)),
            new_int(ec.heap, BigInt::from(3)),
        ];
        let result = lt_b(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(true)));

        // Test not strictly increasing
        let args = vec![
            new_int(ec.heap, BigInt::from(1)),
            new_int(ec.heap, BigInt::from(2)),
            new_int(ec.heap, BigInt::from(2)),
        ];
        let result = lt_b(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(false)));

        // Test mixed int and float
        let args = vec![
            new_int(ec.heap, BigInt::from(1)),
            new_float(ec.heap, 1.5),
            new_int(ec.heap, BigInt::from(2)),
        ];
        let result = lt_b(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(true)));

        // Test decreasing sequence
        let args = vec![
            new_int(ec.heap, BigInt::from(3)),
            new_int(ec.heap, BigInt::from(2)),
            new_int(ec.heap, BigInt::from(1)),
        ];
        let result = lt_b(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(false)));
    }

    #[test]
    fn test_gt_builtin() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);

        // Test strictly decreasing integers
        let args = vec![
            new_int(ec.heap, BigInt::from(3)),
            new_int(ec.heap, BigInt::from(2)),
            new_int(ec.heap, BigInt::from(1)),
        ];
        let result = gt_b(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(true)));

        // Test not strictly decreasing
        let args = vec![
            new_int(ec.heap, BigInt::from(3)),
            new_int(ec.heap, BigInt::from(2)),
            new_int(ec.heap, BigInt::from(2)),
        ];
        let result = gt_b(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(false)));

        // Test mixed int and float
        let args = vec![
            new_int(ec.heap, BigInt::from(3)),
            new_float(ec.heap, 2.5),
            new_int(ec.heap, BigInt::from(2)),
        ];
        let result = gt_b(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(true)));

        // Test increasing sequence
        let args = vec![
            new_int(ec.heap, BigInt::from(1)),
            new_int(ec.heap, BigInt::from(2)),
            new_int(ec.heap, BigInt::from(3)),
        ];
        let result = gt_b(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(false)));
    }
}
