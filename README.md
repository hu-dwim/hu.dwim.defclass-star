# hu.dwim.defclass-star

## What

It's a DEFCLASS* macro for Common Lisp that helps to reduce the
boilerplate when using CL:DEFMACRO.

## Where

- Mainly [the github page](https://github.com/hu-dwim/hu.dwim.defclass-star)
- There's also [our server](http://dwim.hu/projects/hu.dwim.defclass-star) (kinda stale)

## How

No surprises here, but it's worth mentioning that it's integrated with
[hu.dwim.def](https://github.com/hu-dwim/hu.dwim.def) and
[ContextL](https://github.com/pcostanza/contextl).

## Why

We wanted a more concise syntax, and make the expansion extensible
(see e.g. `*slot-definition-transformer*`,
`*allowed-slot-definition-properties*`,
`*accessor-name-transformer*`, etc).

## Status

Mature.
