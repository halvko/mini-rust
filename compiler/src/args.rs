use std::{borrow::Cow, path::PathBuf};

use const_format::formatcp;

pub(crate) struct Configuration {
    pub(crate) source_path: PathBuf,
    pub(crate) out_path: PathBuf,
    pub(crate) out_name: String,
    pub(crate) target: Cow<'static, str>,
    pub(crate) rt_path: PathBuf,
    pub(crate) target_path: PathBuf,
    pub(crate) trap_memory_access: bool,
    pub(crate) optimize: bool,
}

const DEFAULT_RT_PATH: &str = "../runtime";
const DEFAULT_TARGET_PATH: &str = "../target";

const TRAP_MEMORY_ACCESS: &str = "trap_memory_access";
const OUT: &str = "out";
const RT_PATH: &str = "runtime_path";
const TARGET: &str = "target";

pub(crate) fn parse(args: impl Iterator<Item = String>) -> Configuration {
    let mut args = args.skip(1); // Skip name

    let mut source_path = None;
    let mut out_path = None;
    let mut trap_memory_access = true;
    let mut rt_path = None;
    let mut optimize = false;
    let mut target = Cow::Borrowed("x86_64-unknown-linux-gnu");

    while let Some(arg) = args.next() {
        if let Some(arg) = arg.strip_prefix('-') {
            if let Some(arg) = arg.strip_prefix('-') {
                match arg {
                    TRAP_MEMORY_ACCESS => {
                        const ERR_IF_MISSING: &str =
                            formatcp!("expected boolean argument after \"{TRAP_MEMORY_ACCESS}\"");
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
                        const ERR_IF_MISSING: &str = formatcp!("expected path after \"{RT_PATH}\"");
                        rt_path = Some(
                            PathBuf::from(args.next().expect(ERR_IF_MISSING))
                                .canonicalize()
                                .unwrap(),
                        );
                    }
                    TARGET => {
                        const ERR_IF_MISSING: &str =
                            formatcp!("expected target-triple after \"{TARGET}\"");
                        target = args.next().expect(ERR_IF_MISSING).into();
                    }
                    a => {
                        panic!("unexpected argument \"--{a}\"")
                    }
                }
                continue;
            }
            match arg {
                "O" => optimize = true,
                arg => panic!("unexpected argument \"-{arg}\""),
            }
            continue;
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
        optimize,
        target_path,
        target,
    }
}
