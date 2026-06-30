# OKaml

A small OCaml interpreter, including:

- Hindey-Milner type reconstruction,
- let-polymorphism, and
- **shift/reset** operators, with answer-type modifications.

```
$ dune build            # build
$ dune exec bin/main.bc # start repl
```

## TODO

- [ ] pattern matches with `let`.
- [ ] `let ... and ...`, especially mutual recursion.
- [ ] add tuple, especially `unit`.
- [ ] add sequential execution with `;`.
- Known bugs:
  - `bin/fold_right.txt`
    - OchaCaml: ` ('a / 'b -> ('c / 'd -> 'c / 'b) / 'd) / 'd -> ('a list / 'd -> ('c / 'd -> 'c / 'd) / 'd) / 'd`
    - OKaml: `('a / 'b -> ('c / 'b -> 'c / 'b) / 'b) / 'b -> ('a list / 'b -> ('c / 'b -> 'c / 'b) / 'b) / 'b`


## References

1. 浅井健一. [*shift/reset プログラミング入門*.](http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-j.pdf)
2. Kenichiki Asai and Yukiyoshi Kameyama. [*Polymorphic Delimited Continuations*](https://www.logic.cs.tsukuba.ac.jp/~kam/paper/aplas07.pdf)
3. gfngfn. [gfngfn/poly-shift-reset](https://github.com/gfngfn/poly-shift-reset)
4. ymyzk. [CPS Interpreter for STLC + shift/reset in OCaml](https://gist.github.com/ymyzk/b9f1cf4ec3db166872c6028bb40d1c96)
