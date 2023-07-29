//! A tiny test runner for Vulpi based on the `atiny-tests` crate for the Atiny language.

#![feature(path_file_prefix)]
#![feature(test)]

extern crate test;

use std::fs::{self, read_to_string};
use std::path::PathBuf;

use test::{TestDesc, TestDescAndFn, TestName};

const EXTENSION: &str = "vp";

pub mod util;

/// A bunch of golden-tests that are run by the test runner. The test runner will run each test
/// that is inside the directory described inside the entry.
pub struct Test {
    pub directory: &'static str,
    pub run: fn(file_name: PathBuf) -> String,
}

/// The main runner that receives tests and then runs them.
pub fn test_runner(tests: &[&Test]) {
    let Some(opts) = get_test_opts() else { return; };

    let mut rendered = Vec::new();

    for test in tests {
        let directory = std::fs::read_dir(test.directory).unwrap();

        for file in directory.flatten() {
            let (file_name, typ) = util::split_name(&file);

            if typ != EXTENSION {
                continue;
            }

            if file.file_type().unwrap().is_file() {
                rendered.push(create_test_description(file_name, file, test.run));
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

fn create_test_description(
    file_name: String,
    file: fs::DirEntry,
    function: fn(PathBuf) -> String,
) -> TestDescAndFn {
    TestDescAndFn {
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
            test_type: test::TestType::UnitTest,
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
    }
}

fn get_test_opts() -> Option<test::TestOpts> {
    let args = std::env::args().collect::<Vec<_>>();
    let parsed = test::test::parse_opts(&args);
    match parsed {
        Some(Ok(o)) => Some(o),
        Some(Err(msg)) => panic!("{:?}", msg),
        None => None,
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
