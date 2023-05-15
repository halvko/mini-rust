mod mini_defs {
    #[link(name = "simple")]
    extern "C" {
        pub fn mr_main();
    }
}

fn main() {
    unsafe { mini_defs::mr_main() }
}

#[no_mangle]
pub extern "C" fn print(a: i64) {
    println!("{a}")
}
