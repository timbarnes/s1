//! R7RS 6.14: `(scheme process-context)` and `(scheme time)`.

use crate::env::{EnvOps, EnvRef};
use crate::gc::{GcHeap, GcRef, SchemeValue, list_from_slice, new_float, new_int, new_pair, new_string};
use crate::gc_value;
use crate::register_builtin_family;
use std::sync::LazyLock;
use std::time::{Instant, SystemTime, UNIX_EPOCH};

/// The origin of `current-jiffy`: fixed when the builtins are registered.
static START: LazyLock<Instant> = LazyLock::new(Instant::now);

/// Jiffies are nanoseconds.
const JIFFIES_PER_SECOND: u64 = 1_000_000_000;

pub fn register_system_builtins(heap: &mut GcHeap, env: EnvRef) {
    LazyLock::force(&START);
    register_builtin_family!(heap, env,
        "get-environment-variable" => (get_environment_variable, "(get-environment-variable name) The value of environment variable name as a string, or #f if it isn't set"),
        "get-environment-variables" => (get_environment_variables, "(get-environment-variables) The environment variables, as a list of (name . value) string pairs"),
        "current-second" => (current_second, "(current-second) Seconds since the Unix epoch (UTC), as an inexact number"),
        "current-jiffy" => (current_jiffy, "(current-jiffy) Jiffies (nanoseconds) since s1 started, as an exact integer"),
        "jiffies-per-second" => (jiffies_per_second, "(jiffies-per-second) The number of jiffies in a second: 1000000000"),
    );
}

fn no_args(args: &[GcRef], who: &str) -> Result<(), String> {
    if args.is_empty() { Ok(()) } else { Err(format!("{}: expected 0 arguments", who)) }
}

fn get_environment_variable(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let name = match args {
        [name] => match gc_value!(*name) {
            SchemeValue::Str(s) => s.clone(),
            _ => return Err("get-environment-variable: name must be a string".to_string()),
        },
        _ => return Err("get-environment-variable: expected 1 argument".to_string()),
    };
    // A name std::env can't look up (empty, or containing '=' or NUL) is
    // simply not set.
    if name.is_empty() || name.contains(['=', '\0']) {
        return Ok(heap.false_s());
    }
    Ok(match std::env::var_os(&name) {
        Some(value) => new_string(heap, &value.to_string_lossy()),
        None => heap.false_s(),
    })
}

fn get_environment_variables(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    no_args(args, "get-environment-variables")?;
    let mut pairs = Vec::new();
    for (name, value) in std::env::vars_os() {
        let name = new_string(heap, &name.to_string_lossy());
        let value = new_string(heap, &value.to_string_lossy());
        pairs.push(new_pair(heap, name, value));
    }
    Ok(list_from_slice(&pairs, heap))
}

fn current_second(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    no_args(args, "current-second")?;
    let since_epoch = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|_| "current-second: the system clock is before 1970".to_string())?;
    Ok(new_float(heap, since_epoch.as_secs_f64()))
}

fn current_jiffy(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    no_args(args, "current-jiffy")?;
    Ok(new_int(heap, num_bigint::BigInt::from(START.elapsed().as_nanos())))
}

fn jiffies_per_second(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    no_args(args, "jiffies-per-second")?;
    Ok(new_int(heap, num_bigint::BigInt::from(JIFFIES_PER_SECOND)))
}
