# mimium (v2)

[![Test(main)](https://github.com/tomoyanonymous/mimium-rs/actions/workflows/ci.yaml/badge.svg?branch=main)](https://github.com/tomoyanonymous/mimium-rs/actions/workflows/ci.yaml) [![Test(dev)](https://github.com/tomoyanonymous/mimium-rs/actions/workflows/ci.yaml/badge.svg?branch=dev)](https://github.com/tomoyanonymous/mimium-rs/actions/workflows/ci.yaml)

a programming language as an infrastructure for sound and music

---

mimium(MInimal-Musical-medIUM) is a programming language for sound and music.

mimium is made to be an infrastructure for distributing music in a form of a source code, not only a tool for musicians and programmers.

Its semantics are technically inspired from several modern programming languages for sound such as *[Faust](https://faust.grame.fr)* and *[Extempore](https://extemporelang.github.io/)*.. 

A minimal example below generates a sinewave of 440Hz.

```rust
// minimal.mmm
let twopi = 3.141595*2
let sr = 48000
fn dsp(){
  sin(now * 440 * twopi / sr)
}
```
*This repository is a mimium Version 2, all the code base is rewritten in Rust while the original was in C++, and Semantics of the language was re-designed. The code is still very under development.*

## Overview of the new compiler-pipeline

- Source code -> tokenizer -> parser -> AST ↓
- Removing "self" -> Type Inference & SSA Conversion -> MIR(Imperative)
- VM ByteCode Generation

## Roadmap

- [x] Basic Data Types
  - [x] AST
  - [x] MIR
  - [x] VM Instructions
- [ ] Aggregate Types
  - [ ] Tuple (Vector) types
- [ ] Compilers
  - [ ] Stateful Functions
    - [x] Feedback with `self`
    - [ ] Delay
  - [x] Parser
  - [ ] MIR Generation
    - [x] Type Inference
      - [ ] Generics 
    - [x] Code Generation
  - [x] VM Code Generation 
  - [ ] Multi-Stage Computation
- [ ] Runtime
  - [x] Audio Driver Backend
    - [x] CPAL implmentation
  - [ ] Logical Scheduler
  - [ ] Runtime value
    - [ ] `now`
    - [ ] `samplerate`
  - [ ] VM
    - [x] Closure upvalue implementation
    - [ ] Optmizations
      - [ ] Use `SmallVec`
    - [ ] wasm implmentation
- [ ] Module System, Package Manager

other todos:  migrating examples

## [License](LICENSE.md)

The source code is lisenced under [Mozilla Puclic License 2.0](LICENSE.md).

## Acknowledgements

This project is supported by all the contributers, [Sponsors](https://github.com/sponsors/tomoyanonymous), grants and scholarships as follows.

- 2019 Exploratory IT Human Resources Project ([The MITOU Program](https://www.ipa.go.jp/jinzai/mitou/portal_index.html)) by IPA: INFORMATION-TECHNOLOGY PROMOTION AGENCY, Japan.
- Kakehashi Foundation