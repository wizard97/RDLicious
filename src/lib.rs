use lalrpop_util::lalrpop_mod;
use pyo3::prelude::*;

pub mod ast;
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
    let test = "addrmap MyAddrmap {}"; // ComponentNamedDef does not include trailing semicolon
    grammar::ComponentNamedDefParser::new().parse(test).unwrap();
}

#[test]
fn enum_simple() {
    let test = "enum MyEnum { A; B=1; };";
    let root = grammar::RootParser::new().parse(test).unwrap();
    assert_eq!(root.0.len(), 1);
}

#[test]
fn multiple_components_and_enum() {
    let test = r#"
        addrmap A {};
        reg B {};
        enum E { X; Y=10; };
    "#;
    let root = grammar::RootParser::new().parse(test).unwrap();
    for item in &root.0 {
        match item {
            ast::RootItem::Component(c) => println!("Component {}", c.name),
            ast::RootItem::Enum(e) => println!("Enum {}", e.name),
        }
    }
    assert_eq!(root.0.len(), 3);
}

#[test]
fn complicated() {
    let test = r#"
        addrmap A {
            reg {
                field {} field_0;
                field {} field_1;
            } reg_foo;

            mem my_mem {
                mementries = 16;
                memwidth = 32;
            };
            mem_mem mem_inst;
        };
        reg B {
            field {} field_2;
            field {} field_3;
        };
    "#;
    let root = grammar::RootParser::new().parse(test).unwrap();
    assert_eq!(root.0.len(), 2);
}

#[test]
fn super_complicated() {
    let test = r#"

    enum my_enum_t {
        VALUE_0 = 0; // Name for value 0
        VALUE_1 = 1; // Name for value 1
        VALUE_2 = 2; // Name for value 2
    };
    
    enum single_bit_enum_t {
        NO = 0; // Name for value 0
        YES = 1; // Name for value 1
    };
    
    reg my_reg {
        name = "My Register";
        desc = "My register";
        regwidth = 64;
    
        field {
            sw = rw;
            hw = r;
            reset = 0xcafe;
        } plain_16b_field[15:0];
    
        field my_field {
            sw = rw;
            hw = r;
            reset = 0x2;
            encode = my_enum_t;
            desc = "My field";
        };
        my_field field_foo[31:16];
    
        field {
            encode = single_bit_enum_t;
            reset = 0x0;
        } single_bit_field[62:62];
    };
    
    regfile leaf_regfile {
        name = "Leaf Register File";
        desc = "Leaf register file";
    
        my_reg leaf_reg;
    };
    
    regfile my_regfile {
        name = "My Register File";
        desc = "My register file";
    
        my_reg my_reg[4];
    
        leaf_regfile leaf @ 0x80;
    };
    
    addrmap leaf_addrmap {
        name = "Imported";
    
        // Make a complicated addrmap with a bunch of different types of (non-addrmap) children.
    
        // 1. A single register.
        my_reg my_reg @ 0x0;
    
        // 2. A single register file.
        leaf_regfile my_leaf_regfile @ 0x8;
    
        // 3. A single memory.
        mem my_mem {
            mementries = 16;
            memwidth = 32;
        };
        external my_mem my_mem_external @ 0x100;
    
        // 4. An array of memories.
        external my_mem my_mem_array[2] @ 0x200;
    
        // 5. An array of registers.
        my_reg foo[2] @ 0x300;
    
        // 6. A single register file, that contains another register file.
        my_regfile my_regfile_not_leaf @ 0x400;
    
        // 7. An array of register files, that contains another register file.
        my_regfile my_regfile_array[2] @ 0x500;
    };
    
    "#;
    let _root = grammar::RootParser::new().parse(test).unwrap();
}

#[test]
fn expression_basic() {
    use ast::Expr;
    let e = grammar::ExprParser::new().parse("1+2*3").unwrap();
    match e {
        Expr::Binary { op, .. } => assert_eq!(op, "+"),
        _ => panic!(),
    }
}

#[test]
fn expression_complex() {
    let e = grammar::ExprParser::new()
        .parse("1 + 2 * 3 == 7 ? 4 : 5")
        .unwrap();
    match e {
        ast::Expr::Ternary { .. } => {}
        _ => panic!("expected ternary"),
    }
}
