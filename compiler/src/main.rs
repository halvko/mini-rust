use std::{
    borrow::Cow,
    env,
    fs::{self, File},
    path::Path,
    process,
};

mod args;

fn main() {
    let config = args::parse(env::args());

    let input = fs::read_to_string(&config.source_path).unwrap();
    let ast = parse::gen_ast(&input).unwrap();
    let typed_ast = typecheck::type_check(ast);
    let ll_path = config.out_path.with_extension("ll");

    #[allow(clippy::expect_fun_call)]
    let mut llvm_file =
        File::create(&ll_path).expect(&format!("couldn't create {}", ll_path.to_string_lossy()));
    codegen::gen_ir(
        typed_ast,
        &mut llvm_file,
        codegen::Options {
            memory_indirection: config.trap_memory_access,
            target: &config.target,
        },
    )
    .unwrap();

    let ll_path = config.out_path.with_extension("ll");
    let o_path = config.out_path.with_extension("o");
    let s_path = config.out_path.with_extension("s");
    let llc_args = LLCArgs {
        optimize: config.optimize,
        dest_path: &s_path,
        output_type: "asm",
        ll_path: &ll_path,
    };
    run_and_print_command(
        process::Command::new("llc").args(llc_args.args().iter().map(|a| a.as_ref())),
    );
    run_and_print_command(
        process::Command::new("llc").args(
            LLCArgs {
                dest_path: &o_path,
                output_type: "obj",
                ..llc_args
            }
            .args()
            .iter()
            .map(|a| a.as_ref()),
        ),
    );
    let lib_path = config
        .out_path
        .with_file_name(format!("lib{}.a", config.out_name));

    run_and_print_command(process::Command::new("ar").args([
        "rcs",
        lib_path.to_str().unwrap(),
        o_path.to_str().unwrap(),
    ]));

    run_and_print_command(process::Command::new("cp").args([
        lib_path.to_str().unwrap(),
        &format!("{}/mr_obj/libmr.a", config.rt_path.to_str().unwrap()),
    ]));

    run_and_print_command(
        process::Command::new("cargo")
            .current_dir(config.rt_path)
            .env("RUSTFLAGS", format!("--target {}", config.target))
            .args(["b", "-q", "-r"]),
    );

    run_and_print_command(process::Command::new("cp").args([
        &format!("{}/release/runtime", config.target_path.to_str().unwrap()),
        config.out_path.to_str().unwrap(),
    ]));
}

struct LLCArgs<'a> {
    optimize: bool,
    dest_path: &'a Path,
    output_type: &'static str,
    ll_path: &'a Path,
}

impl<'a> LLCArgs<'a> {
    fn args(&self) -> Vec<Cow<'_, str>> {
        let mut clang_args = Vec::new();
        clang_args.extend_from_slice(&[
            "-o".into(),
            self.dest_path.to_str().unwrap().to_owned().into(),
        ]);
        clang_args.extend_from_slice(&["-filetype".into(), self.output_type.into()]);
        if self.optimize {
            clang_args.push("-O2".into());
        }
        clang_args.push(self.ll_path.to_str().unwrap().to_owned().into());
        clang_args
    }
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
