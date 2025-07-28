#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RootItem {
    Component(ComponentDef),
    Enum(EnumDef),
    Struct(StructDef),
    DynPropAssign(DynPropAssign),
    LocalPropAssign(LocalPropAssign),
    Udp(UDPDef),
    ExplicitInst(ExplicitComponentInst),
    Constraint(ConstraintDef),
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
    Signal,
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
    pub data_type: DataType,
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
    Literal(Literal),
    Ident(String),
    EnumLiteral {
        scope: String,
        name: String,
    },
    ArrayLiteral(Vec<Expr>),
    StructLiteral {
        name: String,
        kv: Vec<(String, Expr)>,
    },
    Unary {
        op: UnaryOp,
        rhs: Box<Expr>,
    },
    Reduct {
        op: ReductOp,
        rhs: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Ternary {
        cond: Box<Expr>,
        then_br: Box<Expr>,
        else_br: Box<Expr>,
    },
    CastType {
        ty: CastPrimType,
        expr: Box<Expr>,
    },
    CastWidth {
        width: Box<Expr>,
        expr: Box<Expr>,
    },
    PropRef {
        target: Vec<InstanceRefElem>,
        prop: String,
    },
    Concat(Vec<Expr>),
    Replicate {
        count: Box<Expr>,
        elems: Vec<Expr>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Dec(String),
    Hex(String),
    Verilog { raw: String }, // keep raw; later can normalize width/base/digits
    Bool(bool),
    AccessType(String),
    OnReadType(String),
    OnWriteType(String),
    AddressingType(String),
    PrecedenceType(String),
    Special(String), // fallback / generic
    Str(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
    BitNot,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ReductOp {
    And,
    Or,
    Xor,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Pow,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Shl,
    Shr,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    BitAnd,
    BitXor,
    BitXnor,
    BitOr,
    LogAnd,
    LogOr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CastPrimType {
    Boolean,
    Bit,
    LongInt,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UDPDef {
    pub name: String,
    pub attrs: Vec<UDPAttr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UDPAttr {
    Type { data_type: DataType, is_array: bool },
    Default(Expr),
    Usage(Vec<String>),
    Constraint(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExplicitComponentInst {
    pub inst_type: Option<String>, // external|internal
    pub alias: Option<String>,
    pub comp: ComponentRef,
    pub param_inst: Vec<ParamAssign>, // parameter assignments
    pub instances: Vec<SingleInst>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SingleInst {
    pub name: String,
    pub range: Option<(Expr, Expr)>, // range suffix if present
    pub array_dims: Vec<Expr>,       // list of array dimension expressions
    pub init_expr: Option<Expr>,
    pub addr_expr: Option<Expr>,
    pub stride_expr: Option<Expr>,
    pub align_expr: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ComponentRef {
    Named(String),
    Anonymous(ComponentTypePrimary),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParamAssign {
    pub name: String,
    pub value: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DataType {
    Bit { unsigned: bool },
    LongInt { unsigned: bool },
    String,
    Boolean,
    Number,
    Ref,
    User(String),
    AccessType,
    AddressingType,
    OnReadType,
    OnWriteType,
    Other(String),
}

// ---------------- Constraints (subset) ----------------
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConstraintDef {
    pub name: Option<String>,
    pub elems: Vec<ConstraintElem>,
    pub insts: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConstraintElem {
    Rel { lhs: Expr, op: BinaryOp, rhs: Expr },
    PropAssign { id: String, value: Expr },
    InsideValues { lhs: ConstraintLhs, values: Vec<ConstraintInsideValue> },
    InsideEnum { lhs: ConstraintLhs, enum_id: String },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConstraintLhs {
    This,
    InstanceRef(Vec<InstanceRefElem>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConstraintInsideValue {
    Single(Expr),
    Range(Expr, Expr),
}

// Helper to detect disallowed property references in constant contexts
pub fn expr_contains_propref(e: &Expr) -> bool {
    match e {
        Expr::PropRef { .. } => true,
        Expr::Unary { rhs, .. } | Expr::Reduct { rhs, .. } => expr_contains_propref(rhs),
        Expr::Binary { lhs, rhs, .. } => expr_contains_propref(lhs) || expr_contains_propref(rhs),
        Expr::Ternary { cond, then_br, else_br } => {
            expr_contains_propref(cond)
                || expr_contains_propref(then_br)
                || expr_contains_propref(else_br)
        }
        Expr::CastType { expr, .. } | Expr::CastWidth { expr, .. } => expr_contains_propref(expr),
        Expr::Concat(v) | Expr::ArrayLiteral(v) => v.iter().any(expr_contains_propref),
        Expr::Replicate { count, elems } => {
            expr_contains_propref(count) || elems.iter().any(expr_contains_propref)
        }
        Expr::StructLiteral { kv, .. } => kv.iter().any(|(_, ex)| expr_contains_propref(ex)),
        _ => false,
    }
}
