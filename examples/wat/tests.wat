(module
    (func $adder (param $a i32) (result i32)
        local.get $a
        i32.const 10
        i32.gt_s
        (if (result i32 i32) (then
            i32.const 1
            local.get $a
        ) (else
            local.get $a
            local.get $a
        ))
        i32.add
    )

    (func $main (export "_start") (result i32)
        i32.const 5
        (call $adder)
    )
)