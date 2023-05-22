use std::{
    env::args,
    fs::{self, File},
    path::{self, PathBuf},
    process,
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

    let base_path = {
        let mut bp = input_path.clone();
        bp.pop();
        bp
    };

    let input = fs::read_to_string(&input_path).unwrap();
    let ast = parse::gen_ast(&input).unwrap();
    let typed_ast = typecheck::type_check(ast);
    let ll_path = [&base_path, &format!("{file_name}.ll").into()]
        .iter()
        .collect::<PathBuf>();

    let mut llvm_file = File::create(&ll_path).unwrap();
    codegen::gen_ir(typed_ast, &mut llvm_file).unwrap();

    let o_path = [&base_path, &format!("{file_name}.o").into()]
        .iter()
        .collect::<PathBuf>();

    run_and_print_command(process::Command::new("clang").args([
        "-O2",
        "-c",
        "-o",
        &o_path.to_str().unwrap(),
        &ll_path.to_str().unwrap(),
    ]));
    let lib_path = [&base_path, &format!("lib{file_name}.a").into()]
        .iter()
        .collect::<PathBuf>();
    run_and_print_command(process::Command::new("ar").args([
        "rcs",
        lib_path.to_str().unwrap(),
        o_path.to_str().unwrap(),
    ]));

    run_and_print_command(
        process::Command::new("mv").args([lib_path.to_str().unwrap(), "./runtime/mr_obj/libmr.a"]),
    );

    run_and_print_command(
        process::Command::new("cargo")
            .current_dir("./runtime")
            .args(["b", "-q", "-r"]),
    );

    let exe_path = [&base_path, &file_name.into()].iter().collect::<PathBuf>();
    run_and_print_command(process::Command::new("mv").args([
        "./runtime/target/release/runtime",
        exe_path.to_str().unwrap(),
    ]));
}

fn run_and_print_command(p: &mut process::Command) {
    let output = p.output().unwrap();
    if !output.stderr.is_empty() {
        println!(
            "{} stderr:\n{}",
            p.get_program().to_str().unwrap(),
            String::from_utf8(output.stderr).unwrap()
        )
    }
    if !output.stdout.is_empty() {
        println!(
            "{} stderr:\n{}",
            p.get_program().to_str().unwrap(),
            String::from_utf8(output.stdout).unwrap()
        )
    }
}
