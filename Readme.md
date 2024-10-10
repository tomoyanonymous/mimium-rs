# mimium 

[![Test(main)](https://github.com/tomoyanonymous/mimium-rs/actions/workflows/ci.yaml/badge.svg?branch=main)](https://github.com/tomoyanonymous/mimium-rs/actions/workflows/ci.yaml) [![Test(dev)](https://github.com/tomoyanonymous/mimium-rs/actions/workflows/ci.yaml/badge.svg?branch=dev)](https://github.com/tomoyanonymous/mimium-rs/actions/workflows/ci.yaml)

a programming language as an infrastructure for sound and music

<p style="display:flex; justify-content:center;">
<img src = "mimium_logo_slant.svg" width="300" alt="An icon of the mimium. The word “mimium” is written in small caps, white letters at an angle on a gray diamond-shaped background with a gradient. The vertical bars of the letters are evenly spaced, making it look like a pedestrian crossing." />
</p>

---

mimium(MInimal-Musical-medIUM) is a programming language for sound and music.

mimium is made to be an infrastructure for distributing music in a form of a source code, not only a tool for musicians and programmers.

Its semantics are technically inspired from several modern programming languages for sound such as *[Faust](https://faust.grame.fr)* and *[Extempore](https://extemporelang.github.io/)*.

A minimal example below generates a sinewave of 440Hz.

```rust
// minimal.mmm
let twopi = 3.141595*2.0
let sr = 48000.0
fn dsp(){
  sin(now * 440.0 * twopi / sr)
}
```

A special keyword `self` can be used in function, which is a last return value of the function.
This enables an easy and clean expression of feedback connection of signal chain.

```rust
fn lpf(input,fb){    
     (1.0-fb)*input + fb*self
}
```

You can also write a note-level processing by using `@` operator which specifies the time when the function will be executed. Another special keyword `now` can be used for getting current logical time.
An event scheduling is sample-accurate because the scheduler is driven by an audio driver.

```rust
freq = 440.0
fn noteloop(){
    freq = (freq+1200.0)%4000.0
    noteloop()@(now + 48000.0)
}
```

Also, the language design is based on the call by value lambda calculus, so the higher-order functions are supported to express generative signal graph like replicatiing multiple oscillators like the code below.

```rust
fn replicate(n){
    if (n>0.0){
        let c = replicate(n - 1.0,gen)
        let g = gen()
        |x,rate| {g(x,rate) + c(x+100.0,rate+0.1)}
    }else{
        |x,rate| { 0.0 }
    }
}
```

This repository is for a mimium *version 2*, all the code base is rewritten in Rust while the original was in C++, and Semantics of the language was re-designed. The code is still very under development.

## Installation

TODO

## Contributing

There's no concrete way for contributing to the mimium project for now but any type of contribution (bugfix, code refactoring, documentation, showing the usecases, etc).

(However, because the mimium is still early stage of the development and there's much things to do, the proposal or request for new feature without Pull Request will not be accepted.)

Take a look at [Code of Conduct] before you make contribution.

## [License](LICENSE)

©️ the mimium development community.

The source code is licensed under the [Mozilla Puclic License 2.0 (MPL2.0)](LICENSE).

## Original Author

Tomoya Matsuura/松浦知也 <https://matsuuratomoya.com/en>

## Acknowledgements

This project is supported grants and scholarships as follows.

- 2019 Exploratory IT Human Resources Project ([The MITOU Program](https://www.ipa.go.jp/jinzai/mitou/portal_index.html)) by IPA: INFORMATION-TECHNOLOGY PROMOTION AGENCY, Japan.
- Kakehashi Foundation
- JSPS Kakenhi 23K12059 "Civil Engineering of Music, as a Practice and Critics between Music and Engineering"

### Contributers

This list contains the contributers from v1 development, documentation and financial sponsors(via github sponsor).

#### Source Code Contributions

- [Hiroaki Yutani](https://github.com/yutannihilation)
- [Shinichi Tanaka(t-sin)](https://github.com/t-sin)
- [kyo](https://github.com/syougikakugenn)
- [Inqb8tr-jp](https://github.com/Inqb8tr-jp)
- [zakuro9715](https://github.com/zakuro9715)

#### Other forms of Contributions

- [Baku Hashimoto](https://baku89.com)
- [Yuichi Yogo](https://github.com/yuichkun)
- [Ayumu Nagamatsu](http://ayumu-nagamatsu.com/)
- [zigen](https://horol.org/)


## (Memo) Roadmap toward version 2

- [x] Basic Data Types
  - [x] AST
  - [x] MIR
  - [x] VM Instructions
- [x] Aggregate Types
  - [x] Tuple (Vector) types
- [x] Compilers
  - [x] Stateful Functions
    - [x] Feedback with `self`
    - [x] Delay and mem
  - [x] Parser
  - [x] MIR Generation
    - [x] Type Inference
    - [x] Code Generation
  - [x] VM Code Generation 
- [ ] Runtime
  - [x] Audio Driver Backend
    - [x] CPAL implmentation
  - [x] Logical Scheduler
    - [x] auto de-allocation of unused closure
    - [x] destructive assignment of closure upvalues
    - [x] schedule (`@`) operator
  - [ ] Runtime value
    - [x] `now`
    - [ ] `samplerate`
  - [x] VM
    - [x] Closure upvalue implementation
    - [x] StateStorage implementation
  - [ ] simple file include
  - [x] simple audio file reader function
    - [ ] ~~array(slice) type & automatic interporation~~

### Further Plans

- [ ] Multi-stage computation (Hygienic Macro)
- [ ] Generics
- [ ] Native & WASM backend with Cranelift
- [ ] Module System, Package Manager
- [ ] effect system for managing statefull function and IO

other todos: Migrating examples