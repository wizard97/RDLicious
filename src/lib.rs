use lalrpop_util::lalrpop_mod;
use pyo3::prelude::*;

lalrpop_mod!(grammar);

/// Formats the sum of two numbers as string.
#[pyfunction]
fn sum_as_string(a: usize, b: usize) -> PyResult<String> {
    Ok((a + b).to_string())
}

/// A Python module implemented in Rust.
#[pymodule]
fn systemrdl_rust(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(sum_as_string, m)?)?;
    Ok(())
}

#[test]
fn component() {
    let test = "addrmap MyAddrmap {};";
    grammar::ComponentNamedDefParser::new().parse(test).unwrap();
}
