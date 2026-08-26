//! What the host's linker and assembler want, where the two families
//! disagree.
//!
//! Mach-O and ELF differ in spelling far more than in capability: a C
//! symbol carries a leading underscore in one and not the other, dead
//! stripping and symbol hiding have different flags, the export list is
//! a different file format, `libdl` and `libpthread` are part of
//! libSystem rather than libraries to name, and `.align` counts bytes
//! in one assembler and powers of two in the other.
//!
//! Every one of those differences is a way for a link to fail late and
//! specifically -- or, worse, to succeed while exporting the wrong set.
//! They live here together so that a new link site has one place to ask
//! rather than a GNU spelling to copy.

/// Whether the host links Mach-O.  Everything else here follows from it.
pub const MACHO: bool = cfg!(target_os = "macos");

/// A C function's name as the linker spells it.  Mach-O prefixes an
/// underscore to every C symbol; ELF does not.
pub fn asm_name(sym: &str) -> String {
    if MACHO {
        format!("_{sym}")
    } else {
        sym.to_string()
    }
}

/// Demand a symbol nothing references, so that an archive member
/// defining it is pulled in.  Takes the source name; supplies the
/// linker's spelling.
pub fn undefined(sym: &str) -> String {
    format!("-Wl,-u,{}", asm_name(sym))
}

/// Discard sections nothing reaches.
pub fn dead_strip() -> &'static [&'static str] {
    if MACHO {
        &["-Wl,-dead_strip"]
    } else {
        &["-Wl,--gc-sections"]
    }
}

/// Drop the symbol table from the output.
pub fn strip_symbols() -> &'static [&'static str] {
    // Apple's -s is rejected as unsafe for dynamic libraries; -x drops
    // local symbols and keeps the exported surface, which is what the
    // export list already restricts.
    if MACHO {
        &["-Wl,-x"]
    } else {
        &["-Wl,-s"]
    }
}

/// Bind a shared object's calls to its own definitions, so an intra-.so
/// call does not pay a PLT stub and a GOT load.  Mach-O's two-level
/// namespace does this without being asked.
pub fn local_binding() -> &'static [&'static str] {
    if MACHO {
        &[]
    } else {
        &["-Wl,-Bsymbolic-functions"]
    }
}

/// Refuse output that still has unresolved symbols.  Mach-O already
/// does, and says `-undefined error` is deprecated when asked.
pub fn no_undefined() -> &'static [&'static str] {
    if MACHO {
        &[]
    } else {
        &["-Wl,--no-undefined"]
    }
}

/// The C++ runtime the LLVM libraries were built against.
pub fn cxx_runtime() -> &'static str {
    if MACHO {
        "-lc++"
    } else {
        "-lstdc++"
    }
}

/// Support libraries that llvm-sys's bindings reference and a shared
/// libLLVM does not re-export.  Only a fallback: `llvm-config
/// --system-libs --link-static` answers this for the LLVM actually in
/// use, and these names are a guess at a Debian-shaped one.
pub fn llvm_support_libs() -> &'static [&'static str] {
    if MACHO {
        &["-lzstd", "-lcurses"]
    } else {
        &["-ltinfo", "-lzstd"]
    }
}

/// Where a package manager keeps libraries the compiler driver does not
/// search by default.  Homebrew installs outside the SDK, so a library
/// LLVM was built against can be present and still unfindable.
pub fn lib_search_paths() -> Vec<String> {
    if MACHO {
        ["/opt/homebrew/lib", "/usr/local/lib"]
            .into_iter()
            .filter(|p| std::path::Path::new(p).is_dir())
            .map(|p| format!("-L{p}"))
            .collect()
    } else {
        Vec::new()
    }
}

/// Keep an executable's own symbols visible to what it dlopens.
pub fn export_dynamic() -> &'static [&'static str] {
    if MACHO {
        &["-Wl,-export_dynamic"]
    } else {
        &["-Wl,--export-dynamic"]
    }
}

/// Keep a dependency recorded even if nothing resolves against it yet.
/// Apple's linker has no as-needed pass to turn off.
pub fn no_as_needed() -> &'static [&'static str] {
    if MACHO {
        &[]
    } else {
        &["-Wl,--no-as-needed"]
    }
}

/// dlopen and pthreads: separate libraries under glibc, part of
/// libSystem on Mach-O.
pub fn system_libs() -> &'static [&'static str] {
    if MACHO {
        &[]
    } else {
        &["-lpthread", "-ldl"]
    }
}

/// Name one shared library to link against.  `-l:<file>` is a GNU
/// extension; elsewhere the path itself is the argument.
pub fn link_shared(path: &std::path::Path, soname: &str) -> String {
    if MACHO {
        path.display().to_string()
    } else {
        format!("-l:{soname}")
    }
}

/// Write the export list in the host's format and return the flags that
/// point the linker at it.
///
/// The two formats disagree about more than syntax: a version script
/// names what stays global *and* hides the rest, while an exported
/// symbols list is only the keep-set, with the hiding implied.  Both
/// take globs, and Mach-O wants each pattern in the linker's spelling.
pub fn export_list(
    path: &std::path::Path,
    keep: &[&str],
) -> std::io::Result<Vec<String>> {
    if MACHO {
        let body: String =
            keep.iter().map(|p| format!("{}\n", asm_name(p))).collect();
        std::fs::write(path, body)?;
        Ok(vec![format!("-Wl,-exported_symbols_list,{}", path.display())])
    } else {
        let body = format!("{{ global: {}; local: *; }};\n", keep.join("; "));
        std::fs::write(path, body)?;
        Ok(vec![format!("-Wl,--version-script={}", path.display())])
    }
}

/// An assembly stub that embeds a file and brackets it with two
/// symbols, in the host assembler's dialect.
///
/// Neither the section name nor the alignment directive carries over:
/// Mach-O has no `.rodata` and no GNU-stack note, and its `.align`
/// counts powers of two where the GNU assembler counts bytes, so `8`
/// would mean 256.  `.p2align` means the same thing to both.
pub fn incbin_stub(file: &std::path::Path, start: &str, end: &str) -> String {
    let (sec, note) = if MACHO {
        ("\t.section __TEXT,__const\n", "")
    } else {
        ("\t.section .rodata\n", "\t.section .note.GNU-stack,\"\",@progbits\n")
    };
    format!(
        "{note}{sec}\t.p2align 3\n\
         \t.globl {s}\n\
         {s}:\n\
         \t.incbin \"{f}\"\n\
         \t.globl {e}\n\
         {e}:\n",
        s = asm_name(start),
        e = asm_name(end),
        f = file.display(),
    )
}
