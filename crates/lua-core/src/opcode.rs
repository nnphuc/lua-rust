/// Bytecode instruction set for the lua-rust virtual machine.
///
/// This is a register-based ISA, loosely inspired by the PUC-Lua 5.4 opcode set.
/// Each instruction is a single 32-bit word (encoded format is deferred;
/// for now variants carry typed operands).
#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum OpCode {
    /// Load a constant into register `dst`.
    LoadConst {
        dst: u8,
        const_idx: u16,
    },
    /// Load `nil` into register `dst`.
    LoadNil {
        dst: u8,
    },
    /// Load boolean into register `dst`.  If `skip` is true, skip next instruction.
    LoadBool {
        dst: u8,
        value: bool,
        skip: bool,
    },

    /// Move register `src` into `dst`.
    Move {
        dst: u8,
        src: u8,
    },

    // Arithmetic
    Add {
        dst: u8,
        lhs: u8,
        rhs: u8,
    },
    Sub {
        dst: u8,
        lhs: u8,
        rhs: u8,
    },
    Mul {
        dst: u8,
        lhs: u8,
        rhs: u8,
    },
    Div {
        dst: u8,
        lhs: u8,
        rhs: u8,
    },
    Mod {
        dst: u8,
        lhs: u8,
        rhs: u8,
    },
    Pow {
        dst: u8,
        lhs: u8,
        rhs: u8,
    },
    /// Integer floor division (`//` in Lua)
    IDiv {
        dst: u8,
        lhs: u8,
        rhs: u8,
    },
    /// Unary minus
    Unm {
        dst: u8,
        src: u8,
    },

    // Comparison (result stored as boolean in `dst`)
    Eq {
        dst: u8,
        lhs: u8,
        rhs: u8,
    },
    Lt {
        dst: u8,
        lhs: u8,
        rhs: u8,
    },
    Le {
        dst: u8,
        lhs: u8,
        rhs: u8,
    },

    // Logical
    Not {
        dst: u8,
        src: u8,
    },

    // Control flow
    Jump {
        offset: i16,
    },
    JumpIfFalse {
        src: u8,
        offset: i16,
    },

    // String
    Concat {
        dst: u8,
        start: u8,
        end: u8,
    },
    Len {
        dst: u8,
        src: u8,
    },

    // Functions
    Call {
        func: u8,
        num_args: u8,
        num_results: u8,
    },
    Return {
        src: u8,
        num_results: u8,
    },

    // Global variable access
    GetGlobal {
        dst: u8,
        name_idx: u16,
    },
    SetGlobal {
        src: u8,
        name_idx: u16,
    },

    // Closures & upvalues
    /// Instantiate closure from `proto_table[proto_idx]` in the current chunk.
    Closure {
        dst: u8,
        proto_idx: u16,
    },
    /// Load upvalue `upval_idx` from the current closure into `dst`.
    GetUpvalue {
        dst: u8,
        upval_idx: u8,
    },
    /// Store `src` into upvalue `upval_idx` of the current closure.
    SetUpvalue {
        src: u8,
        upval_idx: u8,
    },
    /// Close all open upvalues whose stack register is >= `from_reg`.
    CloseUpvalues {
        from_reg: u8,
    },
}
