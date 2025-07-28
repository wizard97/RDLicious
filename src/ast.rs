#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RootItem {
    Component(ComponentDef),
    Enum(EnumDef),
    Struct(StructDef),
    DynPropAssign(DynPropAssign),
    LocalPropAssign(LocalPropAssign),
    Udp(UDPDef),
    ExplicitInst(ExplicitComponentInst),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Root(pub Vec<RootItem>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ComponentTypePrimary {
    Addrmap,
    Regfile,
    Reg,
    Field,
    Mem,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ComponentDef {
    pub ctype: ComponentTypePrimary,
    pub name: String,
    pub params: Vec<ParamDecl>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumDef {
    pub name: String,
    pub entries: Vec<EnumEntry>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumEntry {
    pub name: String,
    pub value: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParamDecl {
    pub data_type: String,
    pub name: String,
    pub default: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructDef {
    pub name: String,
    pub base: Option<String>,
    pub elems: Vec<StructElem>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructElem {
    pub ty: String,
    pub name: String,
    pub is_array: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DynPropAssign {
    pub target: Vec<InstanceRefElem>,
    pub prop: String,
    pub value: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LocalPropAssign {
    pub is_default: bool,
    pub prop: String,
    pub value: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InstanceRefElem {
    pub ident: String,
    pub array_suffixes: Vec<Option<Expr>>, // None means unsized '[]'
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Number(String),
    Ident(String),
    Str(String),
    Bool(bool),
    EnumLiteral { scope: String, name: String },
    ArrayLiteral(Vec<Expr>),
    StructLiteral { name: String, kv: Vec<(String, Expr)> },
    Unary { op: String, rhs: Box<Expr> },
    Binary { op: String, lhs: Box<Expr>, rhs: Box<Expr> },
    Ternary { cond: Box<Expr>, then_br: Box<Expr>, else_br: Box<Expr> },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UDPDef {
    pub name: String,
    pub attrs: Vec<UDPAttr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UDPAttr {
    Type { data_type: String, is_array: bool },
    Default(Expr),
    Usage(Vec<String>),
    Constraint(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExplicitComponentInst {
    pub inst_type: Option<String>, // external|internal
    pub alias: Option<String>,
    pub base: String,              // component type name
    pub param_inst: bool,          // whether a parameter instantiation was present
    pub instances: Vec<SingleInst>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SingleInst {
    pub name: String,
    pub has_range: bool,   // true if a [a:b] range present
    pub array_dims: u32,   // number of [] array suffixes
    pub has_init: bool,    // '=' constant_expression present
    pub has_addr: bool,    // '@' constant_expression present
    pub has_incr: bool,    // '+=' constant_expression present
    pub has_mod: bool,     // '%=' constant_expression present
}
