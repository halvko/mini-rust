use std::{
    env::args,
    fs::{self, File},
    path, process,
};

use parse;
fn main() {
    let Some(input_path) = args().skip(1).next() else {
        println!("Expected the first argument to be a path to the file wanted compiled");
        return;
    };
    let input_path = path::PathBuf::from(input_path);
    let input_path = input_path.canonicalize().unwrap();

    let (file_name, _) = input_path
        .file_name()
        .unwrap()
        .to_str()
        .unwrap()
        .split_once('.')
        .unwrap();

    let input = fs::read_to_string(&input_path).unwrap();
    let ast = parse::gen_ast(&input).unwrap();
    let typed_ast = typecheck::type_check(ast);
    let ll_name = format!("{file_name}.ll");
    let mut llvm_file = File::create(&ll_name).unwrap();
    codegen::gen_ir(typed_ast, &mut llvm_file).unwrap();

    let clang = process::Command::new("clang")
        .args(["-c", "-o", &format!("{file_name}.o"), &ll_name])
        .output()
        .unwrap();
    if !clang.stderr.is_empty() {
        println!(
            "clang stderr:\n{}",
            String::from_utf8(clang.stderr).unwrap()
        )
    }
    if !clang.stdout.is_empty() {
        println!(
            "clang stdout:\n{}",
            String::from_utf8(clang.stdout).unwrap()
        )
    }
    let ar = process::Command::new("ar")
        .args([
            "rcs",
            &format!("lib{file_name}.a"),
            &format!("{file_name}.o"),
        ])
        .output()
        .unwrap();
    if !ar.stderr.is_empty() {
        println!("ar stderr:\n{}", String::from_utf8(ar.stderr).unwrap())
    }
    if !ar.stdout.is_empty() {
        println!("ar stdout:\n{}", String::from_utf8(ar.stdout).unwrap())
    }
}
