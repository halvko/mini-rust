use std::{
    env::args,
    fs::{self, File},
    process,
};

mod args {
    use std::path::PathBuf;

    use const_format::formatcp;

    pub(crate) struct Configuration {
        pub(crate) source_path: PathBuf,
        pub(crate) out_path: PathBuf,
        pub(crate) out_name: String,
        pub(crate) rt_path: PathBuf,
        pub(crate) target_path: PathBuf,
        pub(crate) trap_memory_access: bool,
    }

    const DEFAULT_RT_PATH: &str = "../runtime";
    const DEFAULT_TARGET_PATH: &str = "../target";

    const TRAP_MEMORY_ACCESS: &str = "trap_memory_access";
    const OUT: &str = "out";
    const RT_PATH: &str = "runtime_path";

    pub(crate) fn parse(args: impl Iterator<Item = String>) -> Configuration {
        let mut args = args.skip(1); // Skip name

        let mut source_path = None;
        let mut out_path = None;
        let mut trap_memory_access = true;
        let mut rt_path = None;

        while let Some(arg) = args.next() {
            if let Some(arg) = arg.strip_prefix('-') {
                if let Some(arg) = arg.strip_prefix('-') {
                    match arg {
                        TRAP_MEMORY_ACCESS => {
                            const ERR_IF_MISSING: &str = formatcp!(
                                "expected boolean argument after \"{TRAP_MEMORY_ACCESS}\""
                            );
                            let b = args
                                .next()
                                .expect(ERR_IF_MISSING)
                                .parse::<bool>()
                                .expect(ERR_IF_MISSING);
                            trap_memory_access = b;
                        }
                        OUT => {
                            const ERR_IF_MISSING: &str = formatcp!("expected path after \"{OUT}\"");
                            out_path = Some(PathBuf::from(args.next().expect(ERR_IF_MISSING)));
                        }
                        RT_PATH => {
                            const ERR_IF_MISSING: &str =
                                formatcp!("expected path after \"{RT_PATH}\"");
                            rt_path = Some(
                                PathBuf::from(args.next().expect(ERR_IF_MISSING))
                                    .canonicalize()
                                    .unwrap(),
                            );
                        }
                        a => {
                            panic!("unexpected argument \"--{a}\"")
                        }
                    }
                    continue;
                }
                panic!("unexpected argument \"-{arg}\"");
            }
            let sp = PathBuf::from(arg.clone())
                .canonicalize()
                .unwrap_or_else(|_| panic!("could not canonicalize path to {arg}"));
            if !sp.is_file() {
                panic!("expected source path to point to a file")
            }
            if sp
                .extension()
                .expect("expected input file to have extension \".mr\"")
                .to_str()
                .unwrap()
                != "mr"
            {
                panic!("expected input file to have extension \".mr\"")
            }
            source_path = Some(sp);
        }
        let source_path = source_path.expect("source path");
        let (out_name, out_path) = if let Some(out_path) = out_path {
            let file_name = out_path.file_name().unwrap().to_str().unwrap().to_owned();
            let out_path = {
                let mut out_base = out_path.parent().unwrap().canonicalize().unwrap();
                out_base.push(&file_name);
                out_base
            };

            (file_name, out_path)
        } else {
            (
                source_path
                    .file_stem()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_owned(),
                source_path.with_file_name(source_path.file_stem().unwrap()),
            )
        };

        let rt_path = rt_path.unwrap_or_else(|| {
            PathBuf::from(DEFAULT_RT_PATH.to_owned())
                .canonicalize()
                .unwrap()
        });

        let target_path = PathBuf::from(DEFAULT_TARGET_PATH.to_owned())
            .canonicalize()
            .unwrap();

        Configuration {
            source_path,
            out_name,
            out_path,
            trap_memory_access,
            rt_path,
            target_path,
        }
    }
}

fn main() {
    let config = args::parse(args());

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
        },
    )
    .unwrap();

    let o_path = config.out_path.with_extension("o");
    run_and_print_command(process::Command::new("clang").args([
        "-O2",
        "-c",
        "-o",
        o_path.to_str().unwrap(),
        ll_path.to_str().unwrap(),
    ]));
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
            .env("RUSTFLAGS", "-C target-cpu=native")
            .args(["b", "-q", "-r"]),
    );

    run_and_print_command(process::Command::new("cp").args([
        &format!("{}/release/runtime", config.target_path.to_str().unwrap()),
        config.out_path.to_str().unwrap(),
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
