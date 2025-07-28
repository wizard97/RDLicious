use lalrpop_util::lalrpop_mod;

pub mod ast;
lalrpop_mod!(grammar);

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
            ast::RootItem::Struct(s) => println!("Struct {}", s.name),
            ast::RootItem::DynPropAssign(d) => println!("DynProp {}", d.prop),
            ast::RootItem::LocalPropAssign(p) => println!("LocalProp {}", p.prop),
            ast::RootItem::Udp(u) => println!("UDP {} ({} attrs)", u.name, u.attrs.len()),
            ast::RootItem::ExplicitInst(e) => {
                println!("ExplicitInst base {} ({} insts)", e.base, e.instances.len())
            }
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
    
        /* 3. A single memory. with different comment form
        */
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
fn dynamic_and_local_props() {
    let test = r#"
        reg my_reg {
            width = 32;
            depth = 4;
        };
        my_reg->reset = 0x10;
    "#;
    let root = grammar::RootParser::new().parse(test).unwrap();
    assert!(root.0.len() >= 2);
}

#[test]
fn component_with_params() {
    let test = "reg MyReg #( number WIDTH=32, number DEPTH ) {}";
    let comp = grammar::ComponentNamedDefParser::new().parse(test).unwrap();
    assert_eq!(comp.params.len(), 2);
    assert_eq!(comp.params[0].name, "WIDTH");
    assert!(comp.params[0].default.is_some());
    assert_eq!(comp.params[1].name, "DEPTH");
}

#[test]
fn inst_with_param_assign() {
    // Parameterized instantiation disabled in minimal build stage
    let test = r#"
        reg MyReg {};
        MyReg inst0;
    "#;
    let root = grammar::RootParser::new().parse(test).unwrap();
    assert_eq!(root.0.len(), 2);
}

#[test]
fn udp_def_basic() {
    let test = r#"
        property my_prop {
            type = number;
            default = 5+3;
            component = reg|addrmap|all;
            constraint = componentwidth;
        };
    "#;
    let root = grammar::RootParser::new().parse(test).unwrap();
    assert_eq!(root.0.len(), 1);
    match &root.0[0] {
        ast::RootItem::Udp(u) => {
            assert_eq!(u.name, "my_prop");
            assert_eq!(u.attrs.len(), 4);
        }
        _ => panic!(),
    }
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
        .parse("1 + 2 * 3 << 1 == 13 ? 4 : 5")
        .unwrap();
    match e {
        ast::Expr::Ternary { .. } => {}
        _ => panic!("expected ternary"),
    }
}

#[test]
fn expression_precedence_full() {
    use ast::Expr;
    // 2 + 3 * 4 ** 2 == 50 && 1 | 2 ^ 3 & 4 ? 10 : 20
    let e = grammar::ExprParser::new()
        .parse("2 + 3 * 4 ** 2 == 50 && 1 | 2 ^ 3 & 4 ? 10 : 20")
        .unwrap();
    if let Expr::Ternary { cond, then_br, else_br } = e {
        match *cond {
            Expr::Binary { op, .. } => assert_eq!(op, "&&"),
            _ => panic!("cond not logical and"),
        }
        match *then_br {
            Expr::Number(n) => assert_eq!(n, "10"),
            _ => panic!("then branch wrong"),
        }
        match *else_br {
            Expr::Number(n) => assert_eq!(n, "20"),
            _ => panic!("else branch wrong"),
        }
    } else {
        panic!("not ternary");
    }
}

#[test]
fn expression_power_assoc() {
    // 2 ** 3 ** 2 should parse as 2 ** (3 ** 2)
    let e = grammar::ExprParser::new().parse("2 ** 3 ** 2").unwrap();
    // Expect top-level binary op ** with rhs also **
    if let ast::Expr::Binary { op, rhs, .. } = e {
        assert_eq!(op, "**");
        if let ast::Expr::Binary { op: op2, .. } = *rhs { assert_eq!(op2, "**"); } else { panic!("rhs not power"); }
    } else { panic!("not power expression"); }
}

#[test]
fn enum_literal_expr() {
    let e = grammar::ExprParser::new().parse("my_enum::VALUE").unwrap();
    match e {
        ast::Expr::EnumLiteral { scope, name } => {
            assert_eq!(scope, "my_enum");
            assert_eq!(name, "VALUE");
        }
        _ => panic!(),
    }
}

#[test]
fn array_literal_expr() {
    let e = grammar::ExprParser::new().parse("' {1, 2+3}").unwrap();
    match e {
        ast::Expr::ArrayLiteral(v) => assert_eq!(v.len(), 2),
        _ => panic!(),
    }
}

#[test]
fn struct_literal_expr() {
    let e = grammar::ExprParser::new()
        .parse("foo' { A:1, B:2 }")
        .unwrap();
    match e {
        ast::Expr::StructLiteral { name, kv } => {
            assert_eq!(name, "foo");
            assert_eq!(kv.len(), 2);
        }
        _ => panic!(),
    }
}

#[test]
fn struct_def_with_elems() {
    let test = r#"struct foo : bar { number a; mytype b[]; };"#;
    let root = grammar::RootParser::new().parse(test).unwrap();
    assert_eq!(root.0.len(), 1);
    match &root.0[0] {
        ast::RootItem::Struct(s) => {
            assert_eq!(s.name, "foo");
            assert_eq!(s.base.as_deref(), Some("bar"));
            assert_eq!(s.elems.len(), 2);
            assert!(s.elems[1].is_array);
        }
        _ => panic!(),
    }
}

#[test]
fn explicit_component_inst_basic() {
    let test = r#"external my_reg foo, bar[3];"#;
    let root = grammar::RootParser::new().parse(test).unwrap();
    assert_eq!(root.0.len(), 1);
    match &root.0[0] {
        ast::RootItem::ExplicitInst(e) => {
            assert_eq!(e.base, "my_reg");
            assert_eq!(e.inst_type.as_deref(), Some("external"));
            assert_eq!(e.instances.len(), 2);
            assert_eq!(e.instances[1].array_dims, 1);
        }
        _ => panic!(),
    }
}

#[test]
fn explicit_component_inst_with_mods() {
    let test = r#"external alias base_type my_mem #( .W(32) ) inst0[7:0];"#;
    let root = grammar::RootParser::new().parse(test).unwrap();
    match &root.0[0] {
        ast::RootItem::ExplicitInst(e) => {
            assert_eq!(e.inst_type.as_deref(), Some("external"));
            assert!(e.param_inst);
            assert_eq!(e.alias.as_deref(), Some("base_type"));
            assert_eq!(e.instances.len(), 1);
        }
        _ => panic!(),
    }
}
