# Welcome to Flea Scheme
Flea Scheme is a very small toy Scheme implementation that is meant to be a
playground to explore linear logic. The big idea of Flea Scheme is to invert
the following rule:

> All objects created in the course of a Scheme computation, including
> procedures and continuations, have unlimited extent. No Scheme object is ever
> destroyed.

In Flea Scheme, each binding must be referenced exactly once, and objects are
destroyed as soon as the binding holding the value is referenced. Other than that
the goal is to support as much of the R7RS Scheme specification as possible. So
Flea Scheme is an example of Mandatory Manual Memory Management, or (m . ms) for
short (pronounced M&Ms... ba dum tssh). Think Rust, but more annoying.

Special care does need to be taken in dealing with conditionals. First and
foremost, both arms of a conditional must reference the same bindings. If the
true branch of an `if` expression references the variable `x` but the false
branch does not, then `x` will not be used exactly once in all scenarios.
Secondly, it is very common to need a variable in both the test, as well as the
body of of an `if` expression.

In order to support referencing an object more than once, Flea Scheme does
provide the `dup` primitive, it will copy an existing object without destroying
it. For example in this implementation of FizzBuzz the `x` and `y` variables
are only destroyed when handling the recursion at the end of the procedure, all
other references to them go through `dup`.

```scheme
(define (fizzbuzz x y)
  (display
   (if (= (modulo (dup x) 15) 0)
       "FizzBuzz"
       (if (= (modulo (dup x) 3) 0)
           "Fizz"
           (if (= (modulo (dup x) 5) 0)
               "Buzz"
               (dup x)))))
  (newline)
  (if (< (dup x) (dup y))
      (fizzbuzz (+ x 1) y)
      ;; When done, `x`, `y`, and `fizzbuzz` must be destroyed
      (begin x y fizzbuzz)))
```

# Installation
Flea Scheme was built for the 2020 repl.it Programming Language Jam. To try it
out all you have to do is cruise over to
[https://repl.it/@fleascheme/fleascheme#main.scm](https://repl.it/@fleascheme/fleascheme#main.scm)
and hit the big Run button. Be warned however, that it is a very frustrating
experience at the moment.  Flea Scheme has pretty much only been implemented
to successfully run the FizzBuzz function above. There are several gotchyas.
First, once you define a procedure, that procedure is deleted as soon as it is
applied. Second, if the underlying Biwa Scheme implementation encounters an
error, then the Repl will hang, as there seems to be no way to catch the raised
exception. For example the expression `(+ 'foo 1)` will hang the repl when Biwa
raises an error for a non-numeric argument to the `+` procedure.

# Motivation
Rather than coming up with some fresh new ideas for the Programming Language Jam
Flea Scheme revives some rather stale ideas from 1991 that perhaps sholud have
seen some more love.

The main inspiration for Flea Scheme is Henry Baker's paper titled
"Lively Linear Lisp -- 'Look Ma, No Garbage!'", which was once available here:
[http://www.pipeline.com/~hbaker1/LinearLisp.html](http://www.pipeline.com/~hbaker1/LinearLisp.html).

Linear logic can be helpful in dealing with certain types of data that must be
cleaned up. In addition, because objects are destroyed when they are referenced
a Flea Scheme program comes in Static Single assignment form out of the box. On
top of that it is impossible for two pointers to ever alias each other. These
last two points can be very helpful to an optimizing compiler.

One can also look to Rust, which utilizes Affine Types. Affine Types are
similar to linear types in that they can be referenced at most once. Alexis
Beingessner talks about must use types in Rust over here:
[The Pain Of Real Linear Types in Rust](https://gankra.github.io/blah/linear-rust/#must-be-used-at-least-once)

There are other programming languages that have some form of linear types, but
what would be really cool, would be to be open a repl and just give linear
logic a spin, which is where Flea Scheme comes in. If you know a little bit of
Scheme, you can go experiment with linear logic right now (I hessitate to say
linear types, because Flea Scheme is ultimately dynamically typed as any Scheme
is).

Finally, it would be criminal to discuss linear logic without mentioning the
work of Philip Wadler. He has written on the topic many times:
[http://homepages.inf.ed.ac.uk/wadler/topics/linear-logic.html](http://homepages.inf.ed.ac.uk/wadler/topics/linear-logic.html).
A good place to start for the uninitiated is his paper 
[A taste of linear logic](http://homepages.inf.ed.ac.uk/wadler/topics/linear-logic.html#lineartaste).

# Implementation
Flea Scheme is implemented very similarly to the meta-circular evaluator
presented in chapter of four of SICP. The main deviation is in the handling of
the environment.

While the implementation of Flea Scheme is not meta-circular, it does give a
flavor of what it might be like to program with linear logic were Flea Scheme
complete. This can be seen in the procedures that deal with the environment, 
namely `environment-ref` and `free-variables`. The former searches the
environment for a binding, returning both the binding that is found as well as
the new environment. The latter extracts any free variables from the environment
that are captured by a closure. 

## Environment
In order to ensure that no variables are referenced more than once, each
evaluation step not only returns the result of the evaluation, but also a new
environment created by removing any bindings that are referenced in the
expression. For example if the expression `foo` is referenced with the
environment
```scheme
((foo . 1) (bar . 2) (baz . 3))
```
not only is the value `1` returned, but also the the new environment
```scheme
((bar . 2) (baz . 3))
```

In order to ensure that all variables are referenced at least once, the
environment is verified to be empty after evaluating the last expression in a
procedure's body.

There are a few exceptions:
* For convenience, primitive procedure's provided by the underlying Scheme
  implementation, are not destroyed. So the expression `(+ 1 (+ 2 3))` is ok.
* Flea Scheme provides syntax to reference a binding without destroying it.
  This syntax is named `dup` in honor of another paper by Henry Baker:
  "Linear logic and permutation stacks—the Forth shall be first", in
  which Baker equates Linear Lisp to Forth. A very fascinating read!
* While `dup` will work with procedures as well as other types of data,
  direct recursion is supported without special syntax by simply binding
  the procedure itself before evaluating the procedure's body (if the procedure
  is referenced in the body).

## Closures
Flea Scheme does support closures, however any variables that are captured by
the closure will be removed from the enclosing environment. Therefore, the
following will be an error.

```scheme
(define (foo x)
  (define (bar) x)
  x ; Error here as x has been moved to the closure bar
  )
```

# Future Enhancements
Aside from supporting more of the Scheme specification, there are many ways to
make it easier to work with Flea Scheme.

First, it should be possible to provide static errors for an unused, or over-
used variable. If possible, this would also allow to automatically destroy
unused bindings, giving a bit more of a Rust feeling to the language.

In Linear Lisp, Henry Baker makes extensive use of a destructuring `let` in
order to access both the `car` and `cdr` of a `pair`.
```scheme
(dlet ((first second third . rest) '(1 2 3 4 5 6))
  first)
```
This is much more ergonomic than relying on `car+cdr`. Flea Scheme does use
this destructuring let in its implementation, and it would be handy thing to
include in the language itself.

It would be very interesting to bring the concept of borrowing from
Rust. Both Scheme, and many libraries already have a similar concept. Many
procedures that end with an exclamation mark, `!`, are so called linear update
procedures, in that they invalidate the input data, and referencing
the original binding is undefined behavior.

It would be nice to allow annotations to define the behavior of a binding. That
way the programmer can decide if a given binding should be linear, affine,
inifinite (garbage collected), or anywhere in between.


