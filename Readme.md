# mimium-rs

to be continued


## Overview of the compiler-pipeline

source code -> tokenizer -> parser -> AST(Multi-Stage W-calculus) ↓
Removing "self" -> Type Inference & Check -> Early-Stage Evaluation(Macro Expansion) -> HIR(Single-Stage W-calculus)↓
Closure Removal -> SSA Conversion -> MIR(Imperative)↓
Low-level Code Generation