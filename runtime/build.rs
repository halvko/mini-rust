use std::env;

fn main() {
    println!("cargo:rerun-if-changed=mr_obj/libmr.a");
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    println!("cargo:rustc-link-search={manifest_dir}/mr_obj");
}
