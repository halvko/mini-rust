use const_format::formatcp;
use std::{
    env::args,
    fs::{self, File},
    path::{self, PathBuf},
    process,
};

struct Configuration {
    source_path: path::PathBuf,
    out_path: path::PathBuf,
    out_base: path::PathBuf,
    out_name: String,
    trap_memory_access: bool,
}

const TRAP_MEMORY_ACCESS: &str = "trap_memory_access";
const OUT: &str = "out";

fn parse_args(args: impl Iterator<Item = String>) -> Configuration {
    let mut args = args.skip(1); // Skip name

    let mut source_path = None;
    let mut out_path = None;
    let mut trap_memory_access = true;

    while let Some(arg) = args.next() {
        if let Some(arg) = arg.strip_prefix('-') {
            if let Some(arg) = arg.strip_prefix('-') {
                match arg {
                    TRAP_MEMORY_ACCESS => {
                        const ERR_IF_MISSING: &str =
                            formatcp!("boolean argument after \"{TRAP_MEMORY_ACCESS}\"");
                        let b = args
                            .next()
                            .expect(ERR_IF_MISSING)
                            .parse::<bool>()
                            .expect(ERR_IF_MISSING);
                        trap_memory_access = b;
                    }
                    OUT => {
                        const ERR_IF_MISSING: &str = formatcp!("path after \"{OUT}\"");
                        out_path = Some(PathBuf::from(args.next().expect(ERR_IF_MISSING)));
                    }
                    a => {
                        panic!("unexpected argument \"--{a}\"")
                    }
                }
                continue;
            }
            panic!("unexpected argument \"-{arg}\"");
        }
        source_path = Some(
            PathBuf::from(arg.clone())
                .canonicalize()
                .unwrap_or_else(|_| panic!("Could not canonicalize path to {arg}")),
        );
    }
    let source_path = source_path.expect("source path");
    let out_path = out_path.unwrap_or_else(|| {
        let mut out_base = source_path
            .parent()
            .expect("source path to point to file")
            .to_owned();
        out_base.push(source_path.file_name().unwrap().to_str().unwrap());
        out_base
    });
    let out_base = out_path.parent().unwrap().to_owned();
    let (out_name, _) = out_path
        .file_name()
        .unwrap()
        .to_str()
        .unwrap()
        .split_once('.')
        .unwrap();

    Configuration {
        source_path,
        out_name: out_name.to_owned(),
        out_path,
        out_base,
        trap_memory_access,
    }
}

fn main() {
    let config = parse_args(args());

    let input = fs::read_to_string(&config.source_path).unwrap();
    let ast = parse::gen_ast(&input).unwrap();
    let typed_ast = typecheck::type_check(ast);
    let ll_path = [&config.out_base, &format!("{}.ll", config.out_name).into()]
        .iter()
        .collect::<PathBuf>();

    let mut llvm_file = File::create(&ll_path).unwrap();
    codegen::gen_ir(
        typed_ast,
        &mut llvm_file,
        codegen::Options {
            memory_indirection: config.trap_memory_access,
        },
    )
    .unwrap();

    let o_path = [&config.out_base, &format!("{}.o", config.out_name).into()]
        .iter()
        .collect::<PathBuf>();

    run_and_print_command(process::Command::new("clang").args([
        "-O2",
        "-c",
        "-o",
        o_path.to_str().unwrap(),
        ll_path.to_str().unwrap(),
    ]));
    let lib_path = [
        &config.out_base,
        &format!("lib{}.a", config.out_name).into(),
    ]
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
            .current_dir("../runtime")
            .env("RUSTFLAGS", "-C target-cpu=native")
            .args(["b", "-q", "-r"]),
    );

    let exe_path = config.out_path.iter().collect::<PathBuf>();
    run_and_print_command(
        process::Command::new("mv").args(["../target/release/runtime", exe_path.to_str().unwrap()]),
    );
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
