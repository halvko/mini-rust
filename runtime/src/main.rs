use std::hash::{BuildHasher, Hash, Hasher};

use ahash::HashMapExt;
use hwlocality::{cpu::binding::CpuBindingFlags, objects::types::ObjectType};

mod mini_defs {
    use std::ffi::c_void;

    #[link(name = "mr", kind = "static")]
    extern "C" {
        pub fn mr_main(mm: *mut c_void);
    }
}

pub struct RuntimeManager {
    h: ahash::AHasher,
    original_i64_values: ahash::HashMap<*mut u64, u64>,
    original_i1_values: ahash::HashMap<*mut bool, bool>,
    dirty: bool,
}

impl RuntimeManager {
    fn new(h: ahash::AHasher) -> Self {
        Self {
            h,
            original_i64_values: ahash::HashMap::new(),
            original_i1_values: ahash::HashMap::new(),
            dirty: false,
        }
    }

    fn store_i64(&mut self, value: u64, location: *mut u64) {
        if !self.original_i64_values.contains_key(&location) {
            self.original_i64_values.insert(location, value);
        }
        location.hash(&mut self.h);
        value.hash(&mut self.h);
        unsafe { location.write(value) }
    }

    fn load_i64(&mut self, location: *const u64) -> u64 {
        unsafe { location.read() }
    }

    fn store_i1(&mut self, value: bool, location: *mut bool) {
        let value = if self.dirty { !value } else { value };
        if !self.original_i1_values.contains_key(&location) {
            self.original_i1_values.insert(location, value);
        }
        location.hash(&mut self.h);
        value.hash(&mut self.h);
        unsafe { location.write(value) }
    }

    fn load_i1(&mut self, location: *const bool) -> bool {
        unsafe { location.read() }
    }

    fn restore(&mut self, rs: &ahash::RandomState) {
        for (loc, val) in self.original_i64_values.drain() {
            unsafe { loc.write(val) }
        }
        for (loc, val) in self.original_i1_values.drain() {
            unsafe { loc.write(val) }
        }
        self.h = rs.build_hasher();
        self.dirty = true;
    }
}

fn main() {
    let build_hasher = ahash::RandomState::new();
    let mm = &mut RuntimeManager::new(build_hasher.build_hasher());

    let topology = hwlocality::Topology::new().unwrap();
    let cpu_support = topology.feature_support().cpu_binding().unwrap();

    if !cpu_support.set_thread() {
        panic!()
    }

    let core_depth = topology.depth_or_below_for_type(ObjectType::Core).unwrap();
    let mut cores = topology.objects_at_depth(core_depth);

    let tid = unsafe { libc::pthread_self() };

    let mut bind_to = cores.next().unwrap().cpuset().unwrap().clone();

    bind_to.singlify();

    topology
        .bind_thread_cpu(tid, &bind_to, CpuBindingFlags::THREAD)
        .unwrap();

    unsafe { mini_defs::mr_main(mm as *mut _ as _) };

    let hash = mm.h.finish();
    println!("{hash}");
    mm.restore(&build_hasher);

    bind_to = cores.next().unwrap().cpuset().unwrap().clone();

    bind_to.singlify();

    topology
        .bind_thread_cpu(tid, &bind_to, CpuBindingFlags::THREAD)
        .unwrap();

    unsafe { mini_defs::mr_main(mm as *mut _ as _) };
    let second = mm.h.finish();
    println!("{second}");
    if second == hash {
        println!("Equal hashes between runs")
    } else {
        println!("Corruption in one of the runs!")
    }
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
pub extern "C" fn store_i64(mm: *mut RuntimeManager, value: u64, location: *mut u64) {
    unsafe { &mut *mm }.store_i64(value, location)
}

#[no_mangle]
pub extern "C" fn load_i64(mm: *mut RuntimeManager, location: *const u64) -> u64 {
    unsafe { &mut *mm }.load_i64(location)
}

#[no_mangle]
pub extern "C" fn store_i1(mm: *mut RuntimeManager, value: bool, location: *mut bool) {
    unsafe { &mut *mm }.store_i1(value, location)
}

#[no_mangle]
pub extern "C" fn load_i1(mm: *mut RuntimeManager, location: *const bool) -> bool {
    unsafe { &mut *mm }.load_i1(location)
}
