Super-Quick Lambda Calculus
===========================

Inspired by JT, I decided to see if I could write a simple lambda calculus in
one hour.  Nope.  But I can do it in about 90 minutes.  I expect that if I had
used a parser generator I could have done it within the hour.

I made what I think are a few interesting choices in this implementation.

  * I prefered continuation passing style rather than using standard
    call/return.  This means that the evaluator is completely tail recursive,
    but heap allocates all stack frames.  This is likely less efficient, but
    more general.

  * Every type has an error value.  These error values short circuit where it
    makes sense.  In doing so they ususally become the next error value.  e.g.
    evaluating an error expression yields an error value.

  * I added extra values for binary operators.  Rather than hard code +/-/etc
    as special expressions, I made an initial environment that maps the operator
    tokens to the operator values and they are handled pretty much as any other
    variable.
