# Functorize

Oh know, it's a README, what this do? Good question dear reader.

The intent is to transform modules into functors, so this tool should generate a signature with all the external modules used, that should be usable(after some manual fixes) as a functor parameter.

## Requirements

- esy@0.6.7

## How to use?

Build your project using `dune build @check` then find which modules you want to transform into a functor

```shell
esy run other_project/a.cmt other_project/b.cmt
```
