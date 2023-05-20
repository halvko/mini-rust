fn main() {
    println!("cargo:rerun-if-changed=/home/erikfc/code/rust/mini-rust/");
    println!("cargo:rustc-link-search=/home/erikfc/code/rust/mini-rust/");
}
