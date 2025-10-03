[Home](s1-docs.md)

# Numbers

## `+`

`(+ z1 ...)`

Returns the sum of the arguments.

## `-`

`(- z1 z2 ...)`

With one argument, returns the negative of the argument. With multiple arguments, subtracts the sum of the subsequent arguments from the first argument.

## `*`

`(* z1 ...)`

Returns the product of the arguments.

## `/`

`(/ z1 z2 ...)`

With one argument, returns the reciprocal of the argument. With multiple arguments, divides the first argument by the product of the subsequent arguments.

## `=`

`(= x1 x2 ...)`

Returns `#t` if all arguments are numerically equal, `#f` otherwise.

## `<`

`(< x1 x2 ...)`

Returns `#t` if the arguments are in strictly increasing order, `#f` otherwise.

## `>`

`(> x1 x2 ...)`

Returns `#t` if the arguments are in strictly decreasing order, `#f` otherwise.

## `<=`

`(<= x1 x2 ...)`

Returns `#t` if the arguments are in non-decreasing order, `#f` otherwise.

## `>=`

`(>= x1 x2 ...)`

Returns `#t` if the arguments are in non-increasing order, `#f` otherwise.

## `abs`

`(abs x)`

Returns the absolute value of the argument.

## `quotient`

`(quotient n1 n2)`

Returns the integer quotient of `n1` and `n2`.

## `remainder`

`(remainder n1 n2)`

Returns the integer remainder of `n1` and `n2`.

## `modulo`

`(modulo n1 n2)`

Returns the integer modulo of `n1` and `n2`.

## `numerator`

`(numerator q)`

Returns the numerator of rational `q` (for integers, returns the integer itself).

## `denominator`

`(denominator q)`

Returns the denominator of rational `q` (for integers, returns 1).

## `floor`

`(floor x)`

Returns the largest integer not greater than `x`.

## `ceiling`

`(ceiling x)`

Returns the smallest integer not less than `x`.

## `truncate`

`(truncate x)`

Returns the integer closest to `x` that is not larger in absolute value.

## `round`

`(round x)`

Returns the integer closest to `x`.

## `sqrt`

`(sqrt z)`

Returns the principal square root of `z`.

## `expt`

`(expt z1 z2)`

Returns `z1` raised to the power of `z2`.

## `exp`

`(exp z)`

Returns e raised to the power of `z`.

## `log`

`(log z)`

Returns the natural logarithm of `z`.

## `sin`

`(sin z)`

Returns the sine of `z`.

## `cos`

`(cos z)`

Returns the cosine of `z`.

## `tan`

`(tan z)`

Returns the tangent of `z`.

## `asin`

`(asin z)`

Returns the arcsine of `z`.

## `acos`

`(acos z)`

Returns the arccosine of `z`.

## `atan`

`(atan y x)` or `(atan z)`

With one argument, returns the arctangent of `z`. With two arguments, returns the arctangent of `y/x`.

## `zero?`

`(zero? n)`

Returns `#t` if `n` is zero, `#f` otherwise. Implemented in `s1-core.scm`.

## `positive?`

`(positive? n)`

Returns `#t` if `n` is positive, `#f` otherwise. Implemented in `s1-core.scm`.

## `negative?`

`(negative? n)`

Returns `#t` if `n` is negative, `#f` otherwise. Implemented in `s1-core.scm`.

## `even?`

`(even? n)`

Returns `#t` if `n` is even, `#f` otherwise. Implemented in `s1-core.scm`.

## `odd?`

`(odd? n)`

Returns `#t` if `n` is odd, `#f` otherwise. Implemented in `s1-core.scm`.

## `max`

`(max x1 x2 ...)`

Returns the maximum of its arguments. Implemented in `s1-core.scm`.

## `min`

`(min x1 x2 ...)`

Returns the minimum of its arguments. Implemented in `s1-core.scm`.

## `number->string`

`(number->string n)`

Returns a string representation of `n`. Implemented in `s1-core.scm`.

## `string->number`

`(string->number string)`

Returns a number represented by `string`. Implemented in `s1-core.scm`.

[Home](s1-docs.md)