# OKaml

> *The K of OKaml is the K of Keizoku...*

A small OCaml interpreter, including:

- Hindey-Milner type reconstruction,
- let-polymorphism, and
- **shift/reset** operators, with answer-type modifications.

```
$ dune build            # build
$ dune exec bin/main.bc # start repl
```

## References

1. 浅井健一. [*shift/reset プログラミング入門*.](http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-j.pdf)
2. Kameyama, Yukiyoshi, and Masahiro Kiselyov. [*Axioms for Delimited Continuations in the CPS Hierarchy*.](https://www.logic.cs.tsukuba.ac.jp/~kam/paper/aplas07.pdf)
3. gfngfn. [gfngfn/poly-shift-reset](https://github.com/gfngfn/poly-shift-reset)
4. ymyzk. [CPS Interpreter for STLC + shift/reset in OCaml](https://gist.github.com/ymyzk/b9f1cf4ec3db166872c6028bb40d1c96)
