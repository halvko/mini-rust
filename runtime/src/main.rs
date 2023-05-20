mod mini_defs {
    #[link(name = "simple", kind = "static")]
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

#[no_mangle]
pub extern "C" fn sleep(secs: u64) {
    std::thread::sleep(std::time::Duration::from_secs(secs))
}
