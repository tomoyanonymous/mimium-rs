use mimium_macros::mimium_symbols;

mimium_symbols! {
    EntryPoint {
        dsp,
        global: "_mimium_global",
    }

    BuiltinFn {
        sin,
        cos,
        neg,
        not,
        round,
        floor,
        ceil,
        atan,
        sqrt,
        abs,
        add,
        sub,
        mult,
        div,
        modulo,
        eq,
        ne,
        le,
        lt,
        ge,
        gt,
        pow,
        log,
        atan2,
        max,
        min,
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
