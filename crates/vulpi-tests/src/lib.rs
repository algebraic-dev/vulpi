//! A tiny test runner for vulpi. I got the code (that I wrote in the first place) from the Atiny
//! repository.

#![feature(test)]
extern crate test;

use std::{
    fs::{self, read_to_string, DirEntry},
    path::PathBuf,
};

use test::{TestDesc, TestDescAndFn, TestName};

const EXTENSION: &str = "vp";

pub struct Test {
    pub directory: &'static str,
    pub run: fn(file_name: PathBuf) -> String,
}

pub fn split_name(file: &DirEntry) -> (String, String) {
    let name = file.file_name();
    let path = name.to_string_lossy();
    let mut path = path.split('.').collect::<Vec<_>>();

    let typ = path.pop().unwrap();
    (path.join("."), typ.to_string())
}

pub fn test_runner(tests: &[&Test]) {
    let args = std::env::args().collect::<Vec<_>>();
    let parsed = test::test::parse_opts(&args);

    let opts = match parsed {
        Some(Ok(o)) => o,
        Some(Err(msg)) => panic!("{:?}", msg),
        None => return,
    };

    let mut rendered = Vec::new();

    for test in tests {
        let function = test.run;
        let directory = std::fs::read_dir(test.directory).unwrap();

        for file in directory {
            if let Ok(file) = file {
                let (file_name, typ) = split_name(&file);

                if typ != EXTENSION {
                    continue;
                }

                if file.file_type().unwrap().is_file() {
                    rendered.push(TestDescAndFn {
                        desc: TestDesc {
                            name: TestName::DynTestName(file_name.clone()),
                            ignore: false,
                            should_panic: test::ShouldPanic::No,
                            ignore_message: None,
                            source_file: "",
                            start_line: 0,
                            start_col: 0,
                            end_line: 0,
                            end_col: 0,
                            compile_fail: false,
                            no_run: false,
                            test_type: test::TestType::IntegrationTest,
                        },
                        testfn: test::TestFn::DynTestFn(Box::new(move || {
                            println!("testing '{}'", file_name);

                            let path = file.path();

                            let expect_path = path.with_extension("expect");
                            let result = function(path.with_extension(EXTENSION));

                            if let Ok(expects) = read_to_string(expect_path.clone()) {
                                if expects.eq(&result) {
                                    Ok(())
                                } else {
                                    println!("Expected:\n\n{}\n\ngot:\n\n{}", expects, result);
                                    Err("Mismatch".to_string())
                                }
                            } else {
                                fs::write(expect_path, result).map_err(|err| err.to_string())
                            }
                        })),
                    });
                }
            } else {
                break;
            }
        }
    }

    match test::run_tests_console(&opts, rendered) {
        Ok(true) => {
            println!();
        }
        Ok(false) => panic!("some tests failed"),
        Err(e) => panic!("io error when running tests: {:?}", e),
    }
}

#[macro_export]
macro_rules! test {
    ($directory:expr, $code:expr) => {
        #[test_case]
        const TEST: vulpi_tests::Test = vulpi_tests::Test {
            directory: concat!(env!("CARGO_MANIFEST_DIR"), $directory),
            run: $code,
        };
    };
}
