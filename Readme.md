# mimium-rs

to be continued

## Overview of the new compiler-pipeline

- source code -> tokenizer -> parser -> AST(Multi-Stage W-calculus) ↓
- Removing "self" -> Type Inference & Check -> Early-Stage Evaluation(Macro Expansion) -> HIR(Single-Stage W-calculus)↓
- Closure Removal -> SSA Conversion -> MIR(Imperative)↓
- Low-level Code Generation


## Roadmap

- [ ] Basic Data Types
  - [x] AST
  - [x] HIR
  - [ ] MIR
- [ ] Compilers
  - [x] Parser
  - [x] HIR Generation
    - [x] Type Inference (need to test)
  - [ ] MIR Generation
    - [ ] Multi-Stage Removal
    - [ ] Closure Conversion
    - [ ] Code Generation
  - [ ] Code Generation 
- [ ] Runtime
  - [ ] Audio Driver Backend
    - [ ] CPAL implmentation
  - [ ] Logical Scheduler
  - [ ] VM
    - [ ] design
      - [ ] closure upvalue implementation
    - [ ] wasm implmentation
- [ ] FrontEnd
  - [x] HIR treewalk interpreter for test
- [ ] Module System, Package Manager

other todos: intergrated test script, migrating examples