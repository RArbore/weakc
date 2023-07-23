# weakc
`weakc` is a compiler for the weak programming language. `weakc` generates X86-64 assembly that, when linked with `runtime/rt.c`, will produce an executable. `weakc` also includes a reference interpreter, so differential testing is easy.

`weakc` is structured in the following way:

**Source -> Tokens -> AST -> Typed AST -> HIR -> MIR -> X86 (virtual registers) -> X86 assemby**

Lexing and parsing are two separate stages. The type checker adds annotations to the existing AST. A high level intermediate representation is generated from the AST. It is a linear, CFG based IR. The ops in HIR correspond roughly 1:1 with features in the weak language. The mid level intermediate representation is generated from HIR. It is also a linear, CFG based IR. Higher level operations are lowered to operations on pointers and values. X86 code is generated from the MIR, with virtual registers. The register allocator produces X86 code with physical registers, which is then pretty printed at the end.

To use `weakc`, run:
```
cargo run -- -h
```
