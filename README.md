[![Build Status](https://travis-ci.org/sheyll/function-builder.svg?branch=master)](https://travis-ci.org/sheyll/function-builder)

[![Hackage](https://img.shields.io/hackage/v/function-builder.svg?style=flat)](http://hackage.haskell.org/package/function-builder)

# A library for making Monoid writing functions with variable number of parameters

**Also known as: Holey monoids.**

This library is made to be useful especially for library authors, who want to provide users
with building blocks to create functions that compose a monoidal structure
from their parameters in a type safe way. Think of `printf`.

Imagine a library that let`s user create nicely formatted strings, with the ability to
compose strings into larger strings and to allow the rendering of
all sorts of values, for example `Double`s, `Bool`s, or lists to strings.

**This library** allows the author of such a library to easily add the
building blocks, allowing users to build **poly variadic functions**, i.e. with parameters
depending on the order and composition of these building blocks.

Several `FunctionBuilder` values sharing a common monoidal output type can be composed
to a big `FunctionBuilder` value, in order to build an **output function** that
has a flexible number and types of parameters depending, on the individual
`FunctionBuilder`s used. This output function can be obtained by `toFunction`.

`FunctionBuilder`s can also be composed via standard type classes.

This module gives you ready-made  like `Functor`, `Applicative`, `Semigroup`, `Monoid` or Category` instances;

The basic building blocks are `toFunction`, `immediate` and `addParameter`.

For example, you could use this library to build a string formatting
library, that allows users to compose arbitrary, _printf-style_ render **functions**
from reusable building blocks, such that they can be re-combined in order to make
get functions, that can be applied to parameters that fill place holders, like e.g.:

     renderCpuTemp :: Int -> Float -> String
     renderCpuTemp =
       toFunction (render "CPU " . renderInt . render " Temperature: " . renderFloat)

## Similar Libraries

* [polyToMonoid](http://hackage.haskell.org/package/polyToMonoid)

* [HoleyMonoid](http://hackage.haskell.org/package/HoleyMonoid)
