use mimium_macros::mimium_symbols;

mimium_symbols! {
    EntryPoint {
        dsp,
        global: "_mimium_global",
    }

    Intrinsic {
        neg: unary,
        not: unary,

        add: binary,
        sub: binary,
        mult: binary,
        div: binary,
        modulo: binary,
        eq: binary,
        ne: binary,
        le: binary,
        lt: binary,
        ge: binary,
        gt: binary,
        and: binary,
        or: binary,

        sin,
        cos,
        round,
        floor,
        ceil,
        atan,
        sqrt,
        abs,
        pow,
        log,
        atan2,
        max,
        min,
        exp,
    }

    BuiltinFn {
        print,
        println,
        probe,
        probeln,
    }

    SpecialFn {
        delay,
        mem,
    }
}
