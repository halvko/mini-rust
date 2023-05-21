use std::ffi::c_void;

mod mini_defs {
    use std::ffi::c_void;

    #[link(name = "mr", kind = "static")]
    extern "C" {
        pub fn mr_main(mm: *mut c_void);
    }
}

pub struct MemoryManager {}

impl MemoryManager {
    fn new() -> Self {
        Self {}
    }

    pub extern "C" fn store_i64(&mut self, value: u64, location: *mut u64) {
        unsafe { location.write(value) }
    }

    pub extern "C" fn load_i64(&mut self, location: *const u64) -> u64 {
        unsafe { location.read() }
    }

    pub extern "C" fn store_i1(&mut self, value: bool, location: *mut bool) {
        unsafe { location.write(value) }
    }

    pub extern "C" fn load_i1(&mut self, location: *const bool) -> bool {
        unsafe { location.read() }
    }
}

fn main() {
    let mut mm = MemoryManager::new();
    println!("0");
    let mm = &mut mm;
    println!("1");
    let mm: *mut _ = mm;
    println!("2");
    let mm: *mut c_void = mm.cast();
    println!("3");
    unsafe { mini_defs::mr_main(mm) }
}

#[no_mangle]
pub extern "C" fn print(a: i64) {
    println!("{a}")
}

#[no_mangle]
pub extern "C" fn sleep(secs: u64) {
    std::thread::sleep(std::time::Duration::from_secs(secs))
}

#[no_mangle]
pub extern "C" fn store_i64(mm: *mut MemoryManager, value: u64, location: *mut u64) {
    unsafe { &mut *mm }.store_i64(value, location)
}

#[no_mangle]
pub extern "C" fn load_i64(mm: *mut MemoryManager, location: *const u64) -> u64 {
    unsafe { &mut *mm }.load_i64(location)
}

#[no_mangle]
pub extern "C" fn store_i1(mm: *mut MemoryManager, value: bool, location: *mut bool) {
    unsafe { &mut *mm }.store_i1(value, location)
}

#[no_mangle]
pub extern "C" fn load_i1(mm: *mut MemoryManager, location: *const bool) -> bool {
    unsafe { &mut *mm }.load_i1(location)
}
