#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RootItem {
    Component(ComponentDef),
    Enum(EnumDef),
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
pub enum Expr {
    Number(String),
    Ident(String),
    Str(String),
    Unary { op: String, rhs: Box<Expr> },
    Binary { op: String, lhs: Box<Expr>, rhs: Box<Expr> },
    Ternary { cond: Box<Expr>, then_br: Box<Expr>, else_br: Box<Expr> },
}
