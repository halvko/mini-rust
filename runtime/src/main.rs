#![deny(unsafe_op_in_unsafe_fn)]

use std::{
    hash::{BuildHasher, Hash, Hasher},
    mem::MaybeUninit,
    time::Instant,
};

use ahash::HashMapExt;
use hwlocality::{cpu::binding::CpuBindingFlags, objects::types::ObjectType};

mod mini_defs {
    use std::ffi::c_void;

    #[link(name = "mr", kind = "static")]
    extern "C" {
        pub fn mr_main(mm: *mut c_void);
    }
}

/*
struct Allocs<T> {
    allocs: Vec<*mut MaybeUninit<T>>,
    curr: usize,
}

impl<T> Allocs<T> {
    fn new() -> Self {
        Self {
            allocs: Vec::new(),
            curr: 0,
        }
    }
    fn next(&mut self) -> *mut MaybeUninit<T> {
        let ptr = if self.curr >= self.allocs.len() {
            let b = Box::new(MaybeUninit::<T>::uninit());
            let ptr = Box::leak(b) as *mut _;
            self.allocs.push(ptr);
            ptr
        } else {
            self.allocs[self.curr]
        };
        self.curr += 1;
        ptr
    }
}

impl<T> Drop for Allocs<T> {
    fn drop(&mut self) {
        self.allocs
            .drain(..)
            .for_each(|ptr| unsafe { drop(Box::from_raw(ptr)) })
    }
}
*/

pub struct RuntimeManager {
    h: ahash::AHasher,
    testing: bool,
    //allocs: Allocs<i64>,
    original_i64_values: ahash::HashMap<*mut u64, u64>,
    original_i1_values: ahash::HashMap<*mut bool, bool>,
}

impl RuntimeManager {
    fn new(h: ahash::AHasher) -> Self {
        Self {
            h,
            testing: true,
            //allocs: Allocs::new(),
            original_i64_values: ahash::HashMap::new(),
            original_i1_values: ahash::HashMap::new(),
        }
    }

    /*
    fn alloc(&mut self) -> *mut MaybeUninit<i64> {
        self.allocs.next()
    }
    */

    fn persist_side_effects(&mut self, b: bool) {
        self.testing = !b;
    }

    fn print(&mut self, value: i64) {
        value.hash(&mut self.h);
        if self.testing {
            return;
        }
        println!("{value}")
    }

    fn output(&mut self, value: impl Hash) {
        value.hash(&mut self.h);
    }

    fn store_i64(&mut self, value: u64, location: *mut u64) {
        if self.testing {
            self.original_i64_values
                .entry(location)
                .or_insert_with(|| unsafe { location.read() });
        }
        unsafe { location.write(value) }
    }

    fn load_i64(&mut self, location: *const u64) -> u64 {
        unsafe { location.read() }
    }

    fn store_i1(&mut self, value: bool, location: *mut bool) {
        if self.testing {
            self.original_i1_values
                .entry(location)
                .or_insert_with(|| unsafe { location.read() });
        }
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
        //self.allocs.curr = 0;
        self.h = rs.build_hasher();
    }
}

fn main() {
    let start = Instant::now();
    let build_hasher = ahash::RandomState::new();
    let original_hash = build_hasher.build_hasher().finish();
    println!("Hash without work: {original_hash}");
    let mm = &mut RuntimeManager::new(build_hasher.build_hasher());
    let time = Instant::now() - start;
    println!("Setup ran for {}ms", time.as_millis());

    let start = Instant::now();
    let topology = hwlocality::Topology::new().unwrap();
    let cpu_support = topology.feature_support().cpu_binding().unwrap();

    if !cpu_support.set_thread() {
        panic!("System does not support binding the process to a specific cpu")
    }

    let core_depth = topology.depth_or_below_for_type(ObjectType::Core).unwrap();
    let mut cores = topology.objects_at_depth(core_depth);

    let tid = unsafe { libc::pthread_self() };

    let mut bind_to = cores.next().unwrap().cpuset().unwrap().clone();

    bind_to.singlify();

    topology
        .bind_thread_cpu(tid, &bind_to, CpuBindingFlags::THREAD)
        .unwrap();
    let time = Instant::now() - start;
    println!("Bound thread in {} micros", time.as_micros());

    let start = Instant::now();
    unsafe { mini_defs::mr_main(mm as *mut _ as _) };
    let time = Instant::now() - start;
    println!("Ran first run in {} micros", time.as_micros());

    let hash = mm.h.finish();
    println!("{hash}");

    let start = Instant::now();
    mm.restore(&build_hasher);
    mm.persist_side_effects(true);
    let time = Instant::now() - start;
    println!("Restored state in {} micros", time.as_micros());

    let start = Instant::now();
    bind_to = cores.next().unwrap().cpuset().unwrap().clone();

    bind_to.singlify();

    topology
        .bind_thread_cpu(tid, &bind_to, CpuBindingFlags::THREAD)
        .unwrap();
    let time = Instant::now() - start;
    println!("Bound thread in {} micros", time.as_micros());

    let start = Instant::now();
    unsafe { mini_defs::mr_main(mm as *mut _ as _) };
    let time = Instant::now() - start;
    println!("Ran second run in {} micros", time.as_micros());
    let second = mm.h.finish();
    println!("{second}");
    if second == hash {
        println!("Equal hashes between runs")
    } else {
        println!("Corruption in one of the runs!")
    }
}

#[no_mangle]
pub extern "C" fn sleep(_: *mut RuntimeManager, secs: u64) {
    std::thread::sleep(std::time::Duration::from_secs(secs))
}

/// # Safety
///
/// First argument must be an unique aligned pointer to a [RuntimeManager]
#[no_mangle]
pub unsafe extern "C" fn print(mm: *mut RuntimeManager, a: i64) {
    unsafe { &mut *mm }.print(a);
}

/// # Safety
///
/// First argument must be an unique aligned pointer to a [RuntimeManager]
#[no_mangle]
pub unsafe extern "C" fn output_i64(mm: *mut RuntimeManager, val: u64) {
    unsafe { &mut *mm }.output(val);
}

/// # Safety
///
/// First argument must be an unique aligned pointer to a [RuntimeManager]
#[no_mangle]
pub unsafe extern "C" fn output_i1(mm: *mut RuntimeManager, val: bool) {
    unsafe { &mut *mm }.output(val);
}

#[no_mangle]
pub extern "C" fn alloc(_: *mut RuntimeManager) -> *mut MaybeUninit<i64> {
    Box::leak(Box::new(MaybeUninit::uninit())) as *mut _
}

/// # Safety
///
/// `ptr` must be a pointer, created by the alloc function in this module, and may not be accessed
/// in any way after this function returns
#[no_mangle]
pub unsafe extern "C" fn internal_free(_: *mut RuntimeManager, ptr: *mut MaybeUninit<i64>) {
    drop(unsafe { Box::from_raw(ptr) })
}

/// # Safety
///
/// First argument must be an unique aligned pointer to a [RuntimeManager]. Third argument must be
/// an unique aligned pointer to a 64 bit integer
#[no_mangle]
pub unsafe extern "C" fn store_i64(mm: *mut RuntimeManager, value: u64, location: *mut u64) {
    unsafe { &mut *mm }.store_i64(value, location)
}

/// # Safety
///
/// First argument must be an unique aligned pointer to a [RuntimeManager]. Second argument must be
/// an aligned pointer to a 64 bit integer.
#[no_mangle]
pub unsafe extern "C" fn load_i64(mm: *mut RuntimeManager, location: *const u64) -> u64 {
    unsafe { &mut *mm }.load_i64(location)
}

/// # Safety
///
/// First argument must be an unique aligned pointer to a [RuntimeManager], third argument must be
/// an unique aligned pointer to a boolean value, e.g. it must either have the value 0 or 1.
#[no_mangle]
pub unsafe extern "C" fn store_i1(mm: *mut RuntimeManager, value: bool, location: *mut bool) {
    unsafe { &mut *mm }.store_i1(value, location)
}

/// # Safety
///
/// First argument must be an unique aligned pointer to a [RuntimeManager], second argument must be
/// an aligned pointer to a boolean, e.g. it must either have the value 0 or 1.
#[no_mangle]
pub unsafe extern "C" fn load_i1(mm: *mut RuntimeManager, location: *const bool) -> bool {
    unsafe { &mut *mm }.load_i1(location)
}
