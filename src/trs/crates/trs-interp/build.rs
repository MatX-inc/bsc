// Vendored libfst (GTKWave's FST writer, the src/vendor/libfst
// submodule the reference Bluesim links) — compiled here with the
// reference's exact configuration (src/bluesim/Makefile
// FST_LIB_CFLAGS + src/libfst_config/config.h: HAVE_FSEEKO,
// HAVE_REALPATH, no pthread => single-threaded writer) so the
// interp's FST wave sink produces the reference byte-stream through
// the SAME library.  Requires zlib, like the reference kernel.
fn main() {
    let vendor = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../vendor/libfst/src");
    let cfg = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../libfst_config");
    let vendor = vendor.canonicalize().expect(
        "libfst submodule missing — run `git submodule update --init \
         src/vendor/libfst`",
    );
    println!("cargo:rerun-if-changed={}", vendor.display());
    println!("cargo:rerun-if-changed={}", cfg.display());
    cc::Build::new()
        .file(vendor.join("fstapi.c"))
        .file(vendor.join("lz4.c"))
        .file(vendor.join("fastlz.c"))
        .include(&vendor)
        .include(&cfg)
        .flag("-std=gnu99")
        .define("_FILE_OFFSET_BITS", Some("64"))
        .warnings(false)
        .compile("fst");
    println!("cargo:rustc-link-lib=z");
}
