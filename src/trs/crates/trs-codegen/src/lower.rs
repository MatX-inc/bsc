//! BIR -> LLVM IR lowering (feature `llvm`).
//!
//! P2 scope (DESIGN.md §10): single clock domain, registers/wires inlined,
//! flat schedule function, JIT execution.  This module currently only
//! proves out the toolchain wiring (context/module/JIT round-trip).

use inkwell::context::Context;
use inkwell::OptimizationLevel;

/// Smoke-level check that LLVM is usable: build `i64 add(i64,i64)`, JIT it,
/// call it.  Exercised by `cargo test -p trs-codegen --features llvm`.
pub fn llvm_smoke_test() -> Result<u64, String> {
    let ctx = Context::create();
    let module = ctx.create_module("trs_smoke");
    let builder = ctx.create_builder();
    let i64t = ctx.i64_type();
    let fnt = i64t.fn_type(&[i64t.into(), i64t.into()], false);
    let f = module.add_function("add", fnt, None);
    let bb = ctx.append_basic_block(f, "entry");
    builder.position_at_end(bb);
    let a = f.get_nth_param(0).unwrap().into_int_value();
    let b = f.get_nth_param(1).unwrap().into_int_value();
    let sum = builder.build_int_add(a, b, "sum").map_err(|e| e.to_string())?;
    builder.build_return(Some(&sum)).map_err(|e| e.to_string())?;
    let ee = module
        .create_jit_execution_engine(OptimizationLevel::Aggressive)
        .map_err(|e| e.to_string())?;
    let add = unsafe { ee.get_function::<unsafe extern "C" fn(u64, u64) -> u64>("add") }
        .map_err(|e| e.to_string())?;
    Ok(unsafe { add.call(40, 2) })
}

#[cfg(test)]
mod tests {
    #[test]
    fn jit_round_trip() {
        assert_eq!(super::llvm_smoke_test().unwrap(), 42);
    }
}
