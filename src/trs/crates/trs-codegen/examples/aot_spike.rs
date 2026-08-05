//! AOT de-risk spike: emit a PIC object file from an inkwell module,
//! link it into a .so with the system cc, dlopen it, resolve the
//! compiled function and a callback-pointer GLOBAL by name, fill the
//! global with a runtime callback address, and call the function.
//!
//! Proves the three load-bearing assumptions of the persistent-artifact
//! plan (task #11):
//!   1. the same lowering that feeds the JIT engine can emit objects;
//!   2. runtime callbacks work as pointer-globals filled after dlopen
//!      (no --export-dynamic on the host binary);
//!   3. PIC reloc + cc -shared round-trips through dlopen/dlsym.
//!
//! Run: cargo run --release -p trs-codegen --features llvm \
//!        --example aot_spike

use std::ffi::CString;

use inkwell::context::Context;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::{AddressSpace, OptimizationLevel};

extern "C" fn spike_cb(x: u64) -> u64 {
    x * 10
}

fn main() {
    let dir = std::env::temp_dir().join(format!("aot_spike_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let obj = dir.join("spike.o");
    let so = dir.join("libspike.so");

    // -- module: one "sched" fn over an arena + a callback global -----
    let ctx = Context::create();
    let module = ctx.create_module("spike");
    let i64t = ctx.i64_type();
    let ptrt = ctx.ptr_type(AddressSpace::default());

    let cb_global = module.add_global(ptrt, None, "trs_cb_spike");
    cb_global.set_initializer(&ptrt.const_null());

    let fnty = i64t.fn_type(&[ptrt.into()], false);
    let f = module.add_function("sched_spike", fnty, None);
    let entry = ctx.append_basic_block(f, "entry");
    let b = ctx.create_builder();
    b.position_at_end(entry);
    let arena = f.get_nth_param(0).unwrap().into_pointer_value();
    let a0p = unsafe {
        b.build_gep(i64t, arena, &[i64t.const_int(0, false)], "a0p").unwrap()
    };
    let a0 = b.build_load(i64t, a0p, "a0").unwrap().into_int_value();
    let cbp = b
        .build_load(ptrt, cb_global.as_pointer_value(), "cbp")
        .unwrap()
        .into_pointer_value();
    let cbty = i64t.fn_type(&[i64t.into()], false);
    let call = b.build_indirect_call(cbty, cbp, &[a0.into()], "cb").unwrap();
    let inkwell::values::ValueKind::Basic(rv) = call.try_as_basic_value() else {
        panic!("callback call has no return value");
    };
    let rv = rv.into_int_value();
    let inc = b.build_int_add(rv, i64t.const_int(1, false), "inc").unwrap();
    let a1p = unsafe {
        b.build_gep(i64t, arena, &[i64t.const_int(1, false)], "a1p").unwrap()
    };
    b.build_store(a1p, inc).unwrap();
    b.build_return(Some(&inc)).unwrap();
    module.verify().unwrap();

    // -- emit PIC object ----------------------------------------------
    Target::initialize_native(&InitializationConfig::default()).unwrap();
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).unwrap();
    let tm = target
        .create_target_machine(
            &triple,
            &TargetMachine::get_host_cpu_name().to_string(),
            &TargetMachine::get_host_cpu_features().to_string(),
            OptimizationLevel::None,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .unwrap();
    tm.write_to_file(&module, FileType::Object, &obj).unwrap();

    // -- link with the system cc --------------------------------------
    let st = std::process::Command::new("cc")
        .args(["-shared", "-o"])
        .arg(&so)
        .arg(&obj)
        .status()
        .unwrap();
    assert!(st.success(), "cc -shared failed");

    // -- dlopen + resolve + fill callback global + call ----------------
    unsafe {
        let cso = CString::new(so.to_str().unwrap()).unwrap();
        let h = libc::dlopen(cso.as_ptr(), libc::RTLD_NOW);
        assert!(!h.is_null(), "dlopen failed");
        let gname = CString::new("trs_cb_spike").unwrap();
        let g = libc::dlsym(h, gname.as_ptr());
        assert!(!g.is_null(), "dlsym global failed");
        *(g as *mut usize) = spike_cb as usize;
        let fname = CString::new("sched_spike").unwrap();
        let sym = libc::dlsym(h, fname.as_ptr());
        assert!(!sym.is_null(), "dlsym fn failed");
        let sched: unsafe extern "C" fn(*mut u64) -> i64 =
            std::mem::transmute(sym);
        let mut arena = [7u64, 0u64];
        let r = sched(arena.as_mut_ptr());
        assert_eq!(r, 71, "return value");
        assert_eq!(arena[1], 71, "arena store");
    }
    std::fs::remove_dir_all(&dir).ok();
    println!("aot spike OK: object -> cc -shared -> dlopen -> callback-global -> call");
}
