//! Expressions and actions — the `AExpr`/`AAction` analogues
//! (`ASyntax.hs:936-1148`).

use serde::{Deserialize, Serialize};

use crate::StrId;

/// A combinational expression.  Widths are explicit everywhere; values wider
/// than 64 bits carry their constants as little-endian 32-bit limbs (matching
/// today's `WideData` layout and the planned `[n x i32]` state layout).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expr {
    /// Constant. `limbs` is LE 32-bit; width prunes the top limb.
    Const { width: u32, limbs: Vec<u32> },
    /// Reference to a local def.
    Def(StrId),
    /// Reference to a module input port.
    Port(StrId),
    /// Reference to an instantiation parameter.
    Param(StrId),
    /// Value-method call on an instance: `instance.method(args)`.
    MethCall {
        width: u32,
        instance: StrId,
        method: StrId,
        /// Port number for multi-ported methods.
        port: u32,
        args: Vec<Expr>,
    },
    /// The returned value of an ActionValue method; the action side carries
    /// the arguments (split as in `AMethValue`, `ASyntax.hs:1049`).
    MethValue { width: u32, instance: StrId, method: StrId },
    /// Value of an ActionValue foreign task, correlated by cookie
    /// (`ATaskValue`).
    TaskValue { width: u32, cookie: u32 },
    /// Foreign (BDPI) value function call.
    ForeignCall { width: u32, func: StrId, args: Vec<Expr> },
    /// String literal (`ASStr`) — `$display` format strings and the like.
    Str(StrId),
    /// An abstract clock value (`ASClock`) — appears in instantiation
    /// arguments; oscillator and gate expressions.
    Clock { osc: Box<Expr>, gate: Box<Expr> },
    /// An abstract reset value (`ASReset`) — appears in instantiation
    /// arguments.
    Reset { wire: Box<Expr> },
    /// A submodule's output clock gate (`AMGate`).
    Gate { instance: StrId, clock: StrId },
    Prim { op: PrimOp, width: u32, args: Vec<Expr> },
    /// if-then-else / mux.
    If { width: u32, cond: Box<Expr>, then_: Box<Expr>, else_: Box<Expr> },
    /// Case with dense or sparse arms (post `SimPackageOpt.insertCase`).
    Case {
        width: u32,
        scrutinee: Box<Expr>,
        arms: Vec<(u64, Expr)>,
        default: Box<Expr>,
    },
}

impl Expr {
    pub fn width(&self) -> u32 {
        match self {
            Expr::Const { width, .. }
            | Expr::MethCall { width, .. }
            | Expr::MethValue { width, .. }
            | Expr::TaskValue { width, .. }
            | Expr::ForeignCall { width, .. }
            | Expr::Prim { width, .. }
            | Expr::If { width, .. }
            | Expr::Case { width, .. } => *width,
            Expr::Gate { .. } => 1,
            Expr::Clock { .. } | Expr::Reset { .. } => 1,
            // Def/Port/Param/Str widths come from their declarations.
            Expr::Def(_) | Expr::Port(_) | Expr::Param(_) | Expr::Str(_) => 0,
        }
    }
}

/// Primitive operators (the `APrim` subset that survives to the backend).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PrimOp {
    And,
    Or,
    Xor,
    Not,
    Add,
    Sub,
    Mul,
    Quot,
    Rem,
    Neg,
    Eq,
    Ult,
    Ule,
    Slt,
    Sle,
    Shl,
    Lshr,
    Ashr,
    Extract,
    Concat,
    ZeroExt,
    SignExt,
    /// Dynamic bit-select of an array-of-values (post `expandDynSel` this
    /// only appears in forms codegen supports directly).
    Select,
}

/// One statement of a rule or method body: bodies are the exact
/// interleaving of def computations and actions that
/// `tsortActionsAndDefs` produces — a def's position matters, because a
/// later action may mutate state the def reads (the def must see the
/// pre-action value), and computing each def once at its position is
/// what makes shared expression DAGs linear work.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Stmt {
    /// Compute `expr` now and latch it under `name` for this body.  The
    /// expression is the statement's own (post-substitution) form from
    /// tsortActionsAndDefs -- it may differ from the def table's entry
    /// (ActionValue references are substituted with their latched temps,
    /// and inlining can re-embed calls the table still shows).
    Def { name: StrId, expr: Expr },
    Action(Action),
    /// ActionValue call whose result is latched into `def`
    /// (`SFSAssignAction`).
    AvAction { def: StrId, action: Action },
    /// Conditional statement group (`SFSCond`) — e.g. system tasks gated
    /// on the reset wire so `$display` stays quiet during reset.
    Cond { cond: Expr, then_: Vec<Stmt>, else_: Vec<Stmt> },
}

/// An action within a rule or method body (the `AAction` analogue).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Action {
    /// Conditional action-method call: `if (cond) instance.method(args)`.
    MethCall {
        instance: StrId,
        method: StrId,
        port: u32,
        cond: Expr,
        args: Vec<Expr>,
    },
    /// Foreign action call ($display-family or BDPI action).
    Foreign {
        func: StrId,
        cond: Expr,
        args: Vec<Expr>,
        /// Per-arg signed-display flags (`encodeArgs`'s "-" prefix,
        /// `ForeignFunctions.hs:256-262`).
        signed: Vec<bool>,
    },
    /// Foreign ActionValue task; `cookie` links to `Expr::TaskValue`,
    /// `temp` is the def receiving the value (`ATaskAction`).
    Task {
        func: StrId,
        cookie: u32,
        temp: Option<StrId>,
        width: u32,
        cond: Expr,
        args: Vec<Expr>,
        /// Per-arg signed-display flags.
        signed: Vec<bool>,
    },
}
