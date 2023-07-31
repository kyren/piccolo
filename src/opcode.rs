use gc_arena::Collect;

use crate::types::{
    ConstantIndex16, ConstantIndex8, Opt254, PrototypeIndex, RegisterIndex, UpValueIndex, VarCount,
};

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_static)]
pub enum RCIndex {
    Register(RegisterIndex),
    Constant(ConstantIndex8),
}

impl From<RegisterIndex> for RCIndex {
    fn from(value: RegisterIndex) -> Self {
        Self::Register(value)
    }
}

impl From<ConstantIndex8> for RCIndex {
    fn from(value: ConstantIndex8) -> Self {
        Self::Constant(value)
    }
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_static)]
pub enum Operation {
    Move {
        dest: RegisterIndex,
        source: RegisterIndex,
    },
    LoadConstant {
        dest: RegisterIndex,
        constant: ConstantIndex16,
    },
    LoadBool {
        dest: RegisterIndex,
        value: bool,
        // If true, unconditionally skip the next instruction
        skip_next: bool,
    },
    // Load `count` Nil values starting at `dest`
    LoadNil {
        dest: RegisterIndex,
        count: u8,
    },
    NewTable {
        dest: RegisterIndex,
        array_size: u8,
        map_size: u8,
    },
    GetTable {
        dest: RegisterIndex,
        table: RegisterIndex,
        key: RCIndex,
    },
    SetTable {
        table: RegisterIndex,
        key: RCIndex,
        value: RCIndex,
    },
    GetUpTable {
        dest: RegisterIndex,
        table: UpValueIndex,
        key: RCIndex,
    },
    SetUpTable {
        table: UpValueIndex,
        key: RCIndex,
        value: RCIndex,
    },
    /// Set elements of a table as a list.
    ///
    /// Expects the table to be at the `base` index, and it expects a register for the last set
    /// table index at `base + 1`. The register at `base + 1` must *always* be an integer.
    ///
    /// On execution, it will set N values in the table at `base + 1 + n`, offset all of the indexes
    /// by the `base + 1` startind index, and increment the starting index by however many values
    /// were set.
    SetList {
        base: RegisterIndex,
        count: VarCount,
    },
    /// Call the given function with arguments placed after it, invalidates the function register
    /// and all registers above it. When the function returns, the results are placed starting where
    /// the function was located.
    Call {
        func: RegisterIndex,
        args: VarCount,
        returns: VarCount,
    },
    /// Effectively, call the given function with the arguments placed after it, and return the
    /// results of this function to the upper frame. Invalidates the entire current frame and
    /// replaces it with the called function.
    TailCall {
        func: RegisterIndex,
        args: VarCount,
    },
    Return {
        start: RegisterIndex,
        count: VarCount,
    },
    /// Places the contents of the "varargs" at the given register, expecting the given count.
    /// If the count is "variable", then the top of the stack indicates the number of available
    /// arguments.
    VarArgs {
        dest: RegisterIndex,
        count: VarCount,
    },
    Jump {
        offset: i16,
        // If set, close upvalues >= `close_upvalues`
        close_upvalues: Opt254,
    },
    /// Test the register as a boolean, if its boolean value matches `is_true`, skip the next
    /// instruction.
    Test {
        value: RegisterIndex,
        is_true: bool,
    },
    /// Test the value at the `value` register as a boolean, if its boolean value matches `is_true`,
    /// skip the next instruction, otherwise assign the given value (not converted to a boolean) to
    /// the destination register.
    TestSet {
        dest: RegisterIndex,
        value: RegisterIndex,
        is_true: bool,
    },
    Closure {
        dest: RegisterIndex,
        proto: PrototypeIndex,
    },
    /// Used to set up for a numeric for loop:
    ///
    /// R(base) -= R(base + 2)
    /// pc += jump
    NumericForPrep {
        base: RegisterIndex,
        jump: i16,
    },
    /// Used to iterate a numeric for loop:
    ///
    /// R(base) += R(base + 2)
    /// if R(base) <?= R(base + 1) then
    ///     pc += jump
    ///     R(base + 3) = R(base)
    /// end
    ///
    /// The `<?=` operator here means "less than" if the step (aka R(base + 2)) is positive, and
    /// "greater than" if the step is negative
    NumericForLoop {
        base: RegisterIndex,
        jump: i16,
    },
    /// Used to set up for a generic for loop:
    ///
    /// R(base + 3), ..., R(base + 2 + var_count) = R(base)(R(base + 1), R(base + 2))
    GenericForCall {
        base: RegisterIndex,
        var_count: u8,
    },
    /// Used to iterate a generic for loop:
    ///
    /// if R(base + 1) ~= nil then
    ///     R(base) = R(base + 1)
    ///     pc += jump
    /// end
    GenericForLoop {
        base: RegisterIndex,
        jump: i16,
    },
    /// Used for calling methods on tables:
    /// R(base + 1) = R(table)
    /// R(base) = R(table)[RC(key)]
    Method {
        base: RegisterIndex,
        table: RegisterIndex,
        key: RCIndex,
    },
    /// Concatenate the given arguments into a string
    Concat {
        dest: RegisterIndex,
        source: RegisterIndex,
        count: u8,
    },
    GetUpValue {
        dest: RegisterIndex,
        source: UpValueIndex,
    },
    SetUpValue {
        dest: UpValueIndex,
        source: RegisterIndex,
    },
    Length {
        dest: RegisterIndex,
        source: RegisterIndex,
    },
    Eq {
        skip_if: bool,
        left: RCIndex,
        right: RCIndex,
    },
    Less {
        skip_if: bool,
        left: RCIndex,
        right: RCIndex,
    },
    LessEq {
        skip_if: bool,
        left: RCIndex,
        right: RCIndex,
    },
    Not {
        dest: RegisterIndex,
        source: RegisterIndex,
    },
    Minus {
        dest: RegisterIndex,
        source: RegisterIndex,
    },
    Add {
        dest: RegisterIndex,
        left: RCIndex,
        right: RCIndex,
    },
    Sub {
        dest: RegisterIndex,
        left: RCIndex,
        right: RCIndex,
    },
    Mul {
        dest: RegisterIndex,
        left: RCIndex,
        right: RCIndex,
    },
    Div {
        dest: RegisterIndex,
        left: RCIndex,
        right: RCIndex,
    },
    IDiv {
        dest: RegisterIndex,
        left: RCIndex,
        right: RCIndex,
    },
    Mod {
        dest: RegisterIndex,
        left: RCIndex,
        right: RCIndex,
    },
    Pow {
        dest: RegisterIndex,
        left: RCIndex,
        right: RCIndex,
    },
    BitAnd {
        dest: RegisterIndex,
        left: RCIndex,
        right: RCIndex,
    },
    BitOr {
        dest: RegisterIndex,
        left: RCIndex,
        right: RCIndex,
    },
    BitXor {
        dest: RegisterIndex,
        left: RCIndex,
        right: RCIndex,
    },
    ShiftLeft {
        dest: RegisterIndex,
        left: RCIndex,
        right: RCIndex,
    },
    ShiftRight {
        dest: RegisterIndex,
        left: RCIndex,
        right: RCIndex,
    },
    BitNot {
        dest: RegisterIndex,
        source: RegisterIndex,
    },
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_static)]
pub struct OpCode(OpCodeRepr);

impl OpCode {
    pub fn encode(operation: Operation) -> Self {
        Self(match operation {
            Operation::Move { dest, source } => OpCodeRepr::Move { dest, source },
            Operation::LoadConstant { dest, constant } => {
                OpCodeRepr::LoadConstant { dest, constant }
            }
            Operation::LoadBool {
                dest,
                value,
                skip_next,
            } => OpCodeRepr::LoadBool {
                dest,
                value,
                skip_next,
            },
            Operation::LoadNil { dest, count } => OpCodeRepr::LoadNil { dest, count },
            Operation::NewTable {
                dest,
                array_size,
                map_size,
            } => OpCodeRepr::NewTable {
                dest,
                array_size,
                map_size,
            },
            Operation::GetTable { dest, table, key } => match key {
                RCIndex::Register(key) => OpCodeRepr::GetTableR { dest, table, key },
                RCIndex::Constant(key) => OpCodeRepr::GetTableC { dest, table, key },
            },
            Operation::SetTable { table, key, value } => match (key, value) {
                (RCIndex::Register(key), RCIndex::Register(value)) => {
                    OpCodeRepr::SetTableRR { table, key, value }
                }
                (RCIndex::Register(key), RCIndex::Constant(value)) => {
                    OpCodeRepr::SetTableRC { table, key, value }
                }
                (RCIndex::Constant(key), RCIndex::Register(value)) => {
                    OpCodeRepr::SetTableCR { table, key, value }
                }
                (RCIndex::Constant(key), RCIndex::Constant(value)) => {
                    OpCodeRepr::SetTableCC { table, key, value }
                }
            },
            Operation::GetUpTable { dest, table, key } => match key {
                RCIndex::Register(key) => OpCodeRepr::GetUpTableR { dest, table, key },
                RCIndex::Constant(key) => OpCodeRepr::GetUpTableC { dest, table, key },
            },
            Operation::SetUpTable { table, key, value } => match (key, value) {
                (RCIndex::Register(key), RCIndex::Register(value)) => {
                    OpCodeRepr::SetUpTableRR { table, key, value }
                }
                (RCIndex::Register(key), RCIndex::Constant(value)) => {
                    OpCodeRepr::SetUpTableRC { table, key, value }
                }
                (RCIndex::Constant(key), RCIndex::Register(value)) => {
                    OpCodeRepr::SetUpTableCR { table, key, value }
                }
                (RCIndex::Constant(key), RCIndex::Constant(value)) => {
                    OpCodeRepr::SetUpTableCC { table, key, value }
                }
            },
            Operation::SetList { base, count } => OpCodeRepr::SetList { base, count },
            Operation::Call {
                func,
                args,
                returns,
            } => OpCodeRepr::Call {
                func,
                args,
                returns,
            },
            Operation::TailCall { func, args } => OpCodeRepr::TailCall { func, args },
            Operation::Return { start, count } => OpCodeRepr::Return { start, count },
            Operation::VarArgs { dest, count } => OpCodeRepr::VarArgs { dest, count },
            Operation::Jump {
                offset,
                close_upvalues,
            } => OpCodeRepr::Jump {
                offset,
                close_upvalues,
            },
            Operation::Test { value, is_true } => OpCodeRepr::Test { value, is_true },
            Operation::TestSet {
                dest,
                value,
                is_true,
            } => OpCodeRepr::TestSet {
                dest,
                value,
                is_true,
            },
            Operation::Closure { dest, proto } => OpCodeRepr::Closure { dest, proto },
            Operation::NumericForPrep { base, jump } => OpCodeRepr::NumericForPrep { base, jump },
            Operation::NumericForLoop { base, jump } => OpCodeRepr::NumericForLoop { base, jump },
            Operation::GenericForCall { base, var_count } => {
                OpCodeRepr::GenericForCall { base, var_count }
            }
            Operation::GenericForLoop { base, jump } => OpCodeRepr::GenericForLoop { base, jump },
            Operation::Method { base, table, key } => match key {
                RCIndex::Register(key) => OpCodeRepr::MethodR { base, table, key },
                RCIndex::Constant(key) => OpCodeRepr::MethodC { base, table, key },
            },
            Operation::Concat {
                dest,
                source,
                count,
            } => OpCodeRepr::Concat {
                dest,
                source,
                count,
            },
            Operation::GetUpValue { dest, source } => OpCodeRepr::GetUpValue { dest, source },
            Operation::SetUpValue { dest, source } => OpCodeRepr::SetUpValue { dest, source },
            Operation::Length { dest, source } => OpCodeRepr::Length { dest, source },
            Operation::Eq {
                skip_if,
                left,
                right,
            } => match (left, right) {
                (RCIndex::Register(left), RCIndex::Register(right)) => OpCodeRepr::EqRR {
                    skip_if,
                    left,
                    right,
                },
                (RCIndex::Register(left), RCIndex::Constant(right)) => OpCodeRepr::EqRC {
                    skip_if,
                    left,
                    right,
                },
                (RCIndex::Constant(left), RCIndex::Register(right)) => OpCodeRepr::EqCR {
                    skip_if,
                    left,
                    right,
                },
                (RCIndex::Constant(left), RCIndex::Constant(right)) => OpCodeRepr::EqCC {
                    skip_if,
                    left,
                    right,
                },
            },
            Operation::Less {
                skip_if,
                left,
                right,
            } => match (left, right) {
                (RCIndex::Register(left), RCIndex::Register(right)) => OpCodeRepr::LessRR {
                    skip_if,
                    left,
                    right,
                },
                (RCIndex::Register(left), RCIndex::Constant(right)) => OpCodeRepr::LessRC {
                    skip_if,
                    left,
                    right,
                },
                (RCIndex::Constant(left), RCIndex::Register(right)) => OpCodeRepr::LessCR {
                    skip_if,
                    left,
                    right,
                },
                (RCIndex::Constant(left), RCIndex::Constant(right)) => OpCodeRepr::LessCC {
                    skip_if,
                    left,
                    right,
                },
            },
            Operation::LessEq {
                skip_if,
                left,
                right,
            } => match (left, right) {
                (RCIndex::Register(left), RCIndex::Register(right)) => OpCodeRepr::LessEqRR {
                    skip_if,
                    left,
                    right,
                },
                (RCIndex::Register(left), RCIndex::Constant(right)) => OpCodeRepr::LessEqRC {
                    skip_if,
                    left,
                    right,
                },
                (RCIndex::Constant(left), RCIndex::Register(right)) => OpCodeRepr::LessEqCR {
                    skip_if,
                    left,
                    right,
                },
                (RCIndex::Constant(left), RCIndex::Constant(right)) => OpCodeRepr::LessEqCC {
                    skip_if,
                    left,
                    right,
                },
            },
            Operation::Not { dest, source } => OpCodeRepr::Not { dest, source },
            Operation::Minus { dest, source } => OpCodeRepr::Minus { dest, source },
            Operation::Add { dest, left, right } => match (left, right) {
                (RCIndex::Register(left), RCIndex::Register(right)) => {
                    OpCodeRepr::AddRR { dest, left, right }
                }
                (RCIndex::Register(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::AddRC { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Register(right)) => {
                    OpCodeRepr::AddCR { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::AddCC { dest, left, right }
                }
            },
            Operation::Sub { dest, left, right } => match (left, right) {
                (RCIndex::Register(left), RCIndex::Register(right)) => {
                    OpCodeRepr::SubRR { dest, left, right }
                }
                (RCIndex::Register(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::SubRC { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Register(right)) => {
                    OpCodeRepr::SubCR { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::SubCC { dest, left, right }
                }
            },
            Operation::Mul { dest, left, right } => match (left, right) {
                (RCIndex::Register(left), RCIndex::Register(right)) => {
                    OpCodeRepr::MulRR { dest, left, right }
                }
                (RCIndex::Register(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::MulRC { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Register(right)) => {
                    OpCodeRepr::MulCR { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::MulCC { dest, left, right }
                }
            },
            Operation::Div { dest, left, right } => match (left, right) {
                (RCIndex::Register(left), RCIndex::Register(right)) => {
                    OpCodeRepr::DivRR { dest, left, right }
                }
                (RCIndex::Register(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::DivRC { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Register(right)) => {
                    OpCodeRepr::DivCR { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::DivCC { dest, left, right }
                }
            },
            Operation::IDiv { dest, left, right } => match (left, right) {
                (RCIndex::Register(left), RCIndex::Register(right)) => {
                    OpCodeRepr::IDivRR { dest, left, right }
                }
                (RCIndex::Register(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::IDivRC { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Register(right)) => {
                    OpCodeRepr::IDivCR { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::IDivCC { dest, left, right }
                }
            },
            Operation::Mod { dest, left, right } => match (left, right) {
                (RCIndex::Register(left), RCIndex::Register(right)) => {
                    OpCodeRepr::ModRR { dest, left, right }
                }
                (RCIndex::Register(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::ModRC { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Register(right)) => {
                    OpCodeRepr::ModCR { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::ModCC { dest, left, right }
                }
            },
            Operation::Pow { dest, left, right } => match (left, right) {
                (RCIndex::Register(left), RCIndex::Register(right)) => {
                    OpCodeRepr::PowRR { dest, left, right }
                }
                (RCIndex::Register(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::PowRC { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Register(right)) => {
                    OpCodeRepr::PowCR { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::PowCC { dest, left, right }
                }
            },
            Operation::BitAnd { dest, left, right } => match (left, right) {
                (RCIndex::Register(left), RCIndex::Register(right)) => {
                    OpCodeRepr::BitAndRR { dest, left, right }
                }
                (RCIndex::Register(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::BitAndRC { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Register(right)) => {
                    OpCodeRepr::BitAndCR { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::BitAndCC { dest, left, right }
                }
            },
            Operation::BitOr { dest, left, right } => match (left, right) {
                (RCIndex::Register(left), RCIndex::Register(right)) => {
                    OpCodeRepr::BitOrRR { dest, left, right }
                }
                (RCIndex::Register(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::BitOrRC { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Register(right)) => {
                    OpCodeRepr::BitOrCR { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::BitOrCC { dest, left, right }
                }
            },
            Operation::BitXor { dest, left, right } => match (left, right) {
                (RCIndex::Register(left), RCIndex::Register(right)) => {
                    OpCodeRepr::BitXorRR { dest, left, right }
                }
                (RCIndex::Register(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::BitXorRC { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Register(right)) => {
                    OpCodeRepr::BitXorCR { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::BitXorCC { dest, left, right }
                }
            },
            Operation::ShiftLeft { dest, left, right } => match (left, right) {
                (RCIndex::Register(left), RCIndex::Register(right)) => {
                    OpCodeRepr::ShiftLeftRR { dest, left, right }
                }
                (RCIndex::Register(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::ShiftLeftRC { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Register(right)) => {
                    OpCodeRepr::ShiftLeftCR { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::ShiftLeftCC { dest, left, right }
                }
            },
            Operation::ShiftRight { dest, left, right } => match (left, right) {
                (RCIndex::Register(left), RCIndex::Register(right)) => {
                    OpCodeRepr::ShiftRightRR { dest, left, right }
                }
                (RCIndex::Register(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::ShiftRightRC { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Register(right)) => {
                    OpCodeRepr::ShiftRightCR { dest, left, right }
                }
                (RCIndex::Constant(left), RCIndex::Constant(right)) => {
                    OpCodeRepr::ShiftRightCC { dest, left, right }
                }
            },
            Operation::BitNot { dest, source } => OpCodeRepr::BitNot { dest, source },
        })
    }

    pub fn decode(self) -> Operation {
        match self.0 {
            OpCodeRepr::Move { dest, source } => Operation::Move { dest, source },
            OpCodeRepr::LoadConstant { dest, constant } => {
                Operation::LoadConstant { dest, constant }
            }
            OpCodeRepr::LoadBool {
                dest,
                value,
                skip_next,
            } => Operation::LoadBool {
                dest,
                value,
                skip_next,
            },
            OpCodeRepr::LoadNil { dest, count } => Operation::LoadNil { dest, count },
            OpCodeRepr::NewTable {
                dest,
                array_size,
                map_size,
            } => Operation::NewTable {
                dest,
                array_size,
                map_size,
            },
            OpCodeRepr::GetTableR { dest, table, key } => Operation::GetTable {
                dest,
                table,
                key: key.into(),
            },
            OpCodeRepr::GetTableC { dest, table, key } => Operation::GetTable {
                dest,
                table,
                key: key.into(),
            },
            OpCodeRepr::SetTableRR { table, key, value } => Operation::SetTable {
                table,
                key: key.into(),
                value: value.into(),
            },
            OpCodeRepr::SetTableRC { table, key, value } => Operation::SetTable {
                table,
                key: key.into(),
                value: value.into(),
            },
            OpCodeRepr::SetTableCR { table, key, value } => Operation::SetTable {
                table,
                key: key.into(),
                value: value.into(),
            },
            OpCodeRepr::SetTableCC { table, key, value } => Operation::SetTable {
                table,
                key: key.into(),
                value: value.into(),
            },
            OpCodeRepr::GetUpTableR { dest, table, key } => Operation::GetUpTable {
                dest,
                table,
                key: key.into(),
            },
            OpCodeRepr::GetUpTableC { dest, table, key } => Operation::GetUpTable {
                dest,
                table,
                key: key.into(),
            },
            OpCodeRepr::SetUpTableRR { table, key, value } => Operation::SetUpTable {
                table,
                key: key.into(),
                value: value.into(),
            },
            OpCodeRepr::SetUpTableRC { table, key, value } => Operation::SetUpTable {
                table,
                key: key.into(),
                value: value.into(),
            },
            OpCodeRepr::SetUpTableCR { table, key, value } => Operation::SetUpTable {
                table,
                key: key.into(),
                value: value.into(),
            },
            OpCodeRepr::SetUpTableCC { table, key, value } => Operation::SetUpTable {
                table,
                key: key.into(),
                value: value.into(),
            },
            OpCodeRepr::SetList { base, count } => Operation::SetList { base, count },
            OpCodeRepr::Call {
                func,
                args,
                returns,
            } => Operation::Call {
                func,
                args,
                returns,
            },
            OpCodeRepr::TailCall { func, args } => Operation::TailCall { func, args },
            OpCodeRepr::Return { start, count } => Operation::Return { start, count },
            OpCodeRepr::VarArgs { dest, count } => Operation::VarArgs { dest, count },
            OpCodeRepr::Jump {
                offset,
                close_upvalues,
            } => Operation::Jump {
                offset,
                close_upvalues,
            },
            OpCodeRepr::Test { value, is_true } => Operation::Test { value, is_true },
            OpCodeRepr::TestSet {
                dest,
                value,
                is_true,
            } => Operation::TestSet {
                dest,
                value,
                is_true,
            },
            OpCodeRepr::Closure { dest, proto } => Operation::Closure { dest, proto },
            OpCodeRepr::NumericForPrep { base, jump } => Operation::NumericForPrep { base, jump },
            OpCodeRepr::NumericForLoop { base, jump } => Operation::NumericForLoop { base, jump },
            OpCodeRepr::GenericForCall { base, var_count } => {
                Operation::GenericForCall { base, var_count }
            }
            OpCodeRepr::GenericForLoop { base, jump } => Operation::GenericForLoop { base, jump },
            OpCodeRepr::MethodR { base, table, key } => Operation::Method {
                base,
                table,
                key: key.into(),
            },
            OpCodeRepr::MethodC { base, table, key } => Operation::Method {
                base,
                table,
                key: key.into(),
            },
            OpCodeRepr::Concat {
                dest,
                source,
                count,
            } => Operation::Concat {
                dest,
                source,
                count,
            },
            OpCodeRepr::GetUpValue { dest, source } => Operation::GetUpValue { dest, source },
            OpCodeRepr::SetUpValue { dest, source } => Operation::SetUpValue { dest, source },
            OpCodeRepr::Length { dest, source } => Operation::Length { dest, source },
            OpCodeRepr::EqRR {
                skip_if,
                left,
                right,
            } => Operation::Eq {
                skip_if,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::EqRC {
                skip_if,
                left,
                right,
            } => Operation::Eq {
                skip_if,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::EqCR {
                skip_if,
                left,
                right,
            } => Operation::Eq {
                skip_if,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::EqCC {
                skip_if,
                left,
                right,
            } => Operation::Eq {
                skip_if,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::LessRR {
                skip_if,
                left,
                right,
            } => Operation::Less {
                skip_if,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::LessRC {
                skip_if,
                left,
                right,
            } => Operation::Less {
                skip_if,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::LessCR {
                skip_if,
                left,
                right,
            } => Operation::Less {
                skip_if,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::LessCC {
                skip_if,
                left,
                right,
            } => Operation::Less {
                skip_if,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::LessEqRR {
                skip_if,
                left,
                right,
            } => Operation::LessEq {
                skip_if,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::LessEqRC {
                skip_if,
                left,
                right,
            } => Operation::LessEq {
                skip_if,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::LessEqCR {
                skip_if,
                left,
                right,
            } => Operation::LessEq {
                skip_if,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::LessEqCC {
                skip_if,
                left,
                right,
            } => Operation::LessEq {
                skip_if,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::Not { dest, source } => Operation::Not { dest, source },
            OpCodeRepr::Minus { dest, source } => Operation::Minus { dest, source },
            OpCodeRepr::AddRR { dest, left, right } => Operation::Add {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::AddRC { dest, left, right } => Operation::Add {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::AddCR { dest, left, right } => Operation::Add {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::AddCC { dest, left, right } => Operation::Add {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::SubRR { dest, left, right } => Operation::Sub {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::SubRC { dest, left, right } => Operation::Sub {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::SubCR { dest, left, right } => Operation::Sub {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::SubCC { dest, left, right } => Operation::Sub {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::MulRR { dest, left, right } => Operation::Mul {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::MulRC { dest, left, right } => Operation::Mul {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::MulCR { dest, left, right } => Operation::Mul {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::MulCC { dest, left, right } => Operation::Mul {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::DivRR { dest, left, right } => Operation::Div {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::DivRC { dest, left, right } => Operation::Div {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::DivCR { dest, left, right } => Operation::Div {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::DivCC { dest, left, right } => Operation::Div {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::IDivRR { dest, left, right } => Operation::IDiv {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::IDivRC { dest, left, right } => Operation::IDiv {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::IDivCR { dest, left, right } => Operation::IDiv {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::IDivCC { dest, left, right } => Operation::IDiv {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::ModRR { dest, left, right } => Operation::Mod {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::ModRC { dest, left, right } => Operation::Mod {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::ModCR { dest, left, right } => Operation::Mod {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::ModCC { dest, left, right } => Operation::Mod {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::PowRR { dest, left, right } => Operation::Pow {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::PowRC { dest, left, right } => Operation::Pow {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::PowCR { dest, left, right } => Operation::Pow {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::PowCC { dest, left, right } => Operation::Pow {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::BitAndRR { dest, left, right } => Operation::BitAnd {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::BitAndRC { dest, left, right } => Operation::BitAnd {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::BitAndCR { dest, left, right } => Operation::BitAnd {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::BitAndCC { dest, left, right } => Operation::BitAnd {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::BitOrRR { dest, left, right } => Operation::BitOr {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::BitOrRC { dest, left, right } => Operation::BitOr {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::BitOrCR { dest, left, right } => Operation::BitOr {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::BitOrCC { dest, left, right } => Operation::BitOr {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::BitXorRR { dest, left, right } => Operation::BitXor {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::BitXorRC { dest, left, right } => Operation::BitXor {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::BitXorCR { dest, left, right } => Operation::BitXor {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::BitXorCC { dest, left, right } => Operation::BitXor {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::ShiftLeftRR { dest, left, right } => Operation::ShiftLeft {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::ShiftLeftRC { dest, left, right } => Operation::ShiftLeft {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::ShiftLeftCR { dest, left, right } => Operation::ShiftLeft {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::ShiftLeftCC { dest, left, right } => Operation::ShiftLeft {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::ShiftRightRR { dest, left, right } => Operation::ShiftRight {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::ShiftRightRC { dest, left, right } => Operation::ShiftRight {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::ShiftRightCR { dest, left, right } => Operation::ShiftRight {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::ShiftRightCC { dest, left, right } => Operation::ShiftRight {
                dest,
                left: left.into(),
                right: right.into(),
            },
            OpCodeRepr::BitNot { dest, source } => Operation::BitNot { dest, source },
        }
    }
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_static)]
enum OpCodeRepr {
    Move {
        dest: RegisterIndex,
        source: RegisterIndex,
    },
    LoadConstant {
        dest: RegisterIndex,
        constant: ConstantIndex16,
    },
    LoadBool {
        dest: RegisterIndex,
        value: bool,
        skip_next: bool,
    },
    LoadNil {
        dest: RegisterIndex,
        count: u8,
    },
    NewTable {
        dest: RegisterIndex,
        array_size: u8,
        map_size: u8,
    },
    GetTableR {
        dest: RegisterIndex,
        table: RegisterIndex,
        key: RegisterIndex,
    },
    GetTableC {
        dest: RegisterIndex,
        table: RegisterIndex,
        key: ConstantIndex8,
    },
    SetTableRR {
        table: RegisterIndex,
        key: RegisterIndex,
        value: RegisterIndex,
    },
    SetTableRC {
        table: RegisterIndex,
        key: RegisterIndex,
        value: ConstantIndex8,
    },
    SetTableCR {
        table: RegisterIndex,
        key: ConstantIndex8,
        value: RegisterIndex,
    },
    SetTableCC {
        table: RegisterIndex,
        key: ConstantIndex8,
        value: ConstantIndex8,
    },
    GetUpTableR {
        dest: RegisterIndex,
        table: UpValueIndex,
        key: RegisterIndex,
    },
    GetUpTableC {
        dest: RegisterIndex,
        table: UpValueIndex,
        key: ConstantIndex8,
    },
    SetUpTableRR {
        table: UpValueIndex,
        key: RegisterIndex,
        value: RegisterIndex,
    },
    SetUpTableRC {
        table: UpValueIndex,
        key: RegisterIndex,
        value: ConstantIndex8,
    },
    SetUpTableCR {
        table: UpValueIndex,
        key: ConstantIndex8,
        value: RegisterIndex,
    },
    SetUpTableCC {
        table: UpValueIndex,
        key: ConstantIndex8,
        value: ConstantIndex8,
    },
    SetList {
        base: RegisterIndex,
        count: VarCount,
    },
    Call {
        func: RegisterIndex,
        args: VarCount,
        returns: VarCount,
    },
    TailCall {
        func: RegisterIndex,
        args: VarCount,
    },
    Return {
        start: RegisterIndex,
        count: VarCount,
    },
    VarArgs {
        dest: RegisterIndex,
        count: VarCount,
    },
    Jump {
        offset: i16,
        close_upvalues: Opt254,
    },
    Test {
        value: RegisterIndex,
        is_true: bool,
    },
    TestSet {
        dest: RegisterIndex,
        value: RegisterIndex,
        is_true: bool,
    },
    Closure {
        dest: RegisterIndex,
        proto: PrototypeIndex,
    },
    NumericForPrep {
        base: RegisterIndex,
        jump: i16,
    },
    NumericForLoop {
        base: RegisterIndex,
        jump: i16,
    },
    GenericForCall {
        base: RegisterIndex,
        var_count: u8,
    },
    GenericForLoop {
        base: RegisterIndex,
        jump: i16,
    },
    MethodR {
        base: RegisterIndex,
        table: RegisterIndex,
        key: RegisterIndex,
    },
    MethodC {
        base: RegisterIndex,
        table: RegisterIndex,
        key: ConstantIndex8,
    },
    Concat {
        dest: RegisterIndex,
        source: RegisterIndex,
        count: u8,
    },
    GetUpValue {
        dest: RegisterIndex,
        source: UpValueIndex,
    },
    SetUpValue {
        dest: UpValueIndex,
        source: RegisterIndex,
    },
    Length {
        dest: RegisterIndex,
        source: RegisterIndex,
    },
    EqRR {
        skip_if: bool,
        left: RegisterIndex,
        right: RegisterIndex,
    },
    EqRC {
        skip_if: bool,
        left: RegisterIndex,
        right: ConstantIndex8,
    },
    EqCR {
        skip_if: bool,
        left: ConstantIndex8,
        right: RegisterIndex,
    },
    EqCC {
        skip_if: bool,
        left: ConstantIndex8,
        right: ConstantIndex8,
    },
    LessRR {
        skip_if: bool,
        left: RegisterIndex,
        right: RegisterIndex,
    },
    LessRC {
        skip_if: bool,
        left: RegisterIndex,
        right: ConstantIndex8,
    },
    LessCR {
        skip_if: bool,
        left: ConstantIndex8,
        right: RegisterIndex,
    },
    LessCC {
        skip_if: bool,
        left: ConstantIndex8,
        right: ConstantIndex8,
    },
    LessEqRR {
        skip_if: bool,
        left: RegisterIndex,
        right: RegisterIndex,
    },
    LessEqRC {
        skip_if: bool,
        left: RegisterIndex,
        right: ConstantIndex8,
    },
    LessEqCR {
        skip_if: bool,
        left: ConstantIndex8,
        right: RegisterIndex,
    },
    LessEqCC {
        skip_if: bool,
        left: ConstantIndex8,
        right: ConstantIndex8,
    },
    Not {
        dest: RegisterIndex,
        source: RegisterIndex,
    },
    Minus {
        dest: RegisterIndex,
        source: RegisterIndex,
    },
    AddRR {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: RegisterIndex,
    },
    AddRC {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: ConstantIndex8,
    },
    AddCR {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: RegisterIndex,
    },
    AddCC {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: ConstantIndex8,
    },
    SubRR {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: RegisterIndex,
    },
    SubRC {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: ConstantIndex8,
    },
    SubCR {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: RegisterIndex,
    },
    SubCC {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: ConstantIndex8,
    },
    MulRR {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: RegisterIndex,
    },
    MulRC {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: ConstantIndex8,
    },
    MulCR {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: RegisterIndex,
    },
    MulCC {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: ConstantIndex8,
    },
    DivRR {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: RegisterIndex,
    },
    DivRC {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: ConstantIndex8,
    },
    DivCR {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: RegisterIndex,
    },
    DivCC {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: ConstantIndex8,
    },
    IDivRR {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: RegisterIndex,
    },
    IDivRC {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: ConstantIndex8,
    },
    IDivCR {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: RegisterIndex,
    },
    IDivCC {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: ConstantIndex8,
    },
    ModRR {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: RegisterIndex,
    },
    ModRC {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: ConstantIndex8,
    },
    ModCR {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: RegisterIndex,
    },
    ModCC {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: ConstantIndex8,
    },
    PowRR {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: RegisterIndex,
    },
    PowRC {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: ConstantIndex8,
    },
    PowCR {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: RegisterIndex,
    },
    PowCC {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: ConstantIndex8,
    },
    BitAndRR {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: RegisterIndex,
    },
    BitAndRC {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: ConstantIndex8,
    },
    BitAndCR {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: RegisterIndex,
    },
    BitAndCC {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: ConstantIndex8,
    },
    BitOrRR {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: RegisterIndex,
    },
    BitOrRC {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: ConstantIndex8,
    },
    BitOrCR {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: RegisterIndex,
    },
    BitOrCC {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: ConstantIndex8,
    },
    BitXorRR {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: RegisterIndex,
    },
    BitXorRC {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: ConstantIndex8,
    },
    BitXorCR {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: RegisterIndex,
    },
    BitXorCC {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: ConstantIndex8,
    },
    ShiftLeftRR {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: RegisterIndex,
    },
    ShiftLeftRC {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: ConstantIndex8,
    },
    ShiftLeftCR {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: RegisterIndex,
    },
    ShiftLeftCC {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: ConstantIndex8,
    },
    ShiftRightRR {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: RegisterIndex,
    },
    ShiftRightRC {
        dest: RegisterIndex,
        left: RegisterIndex,
        right: ConstantIndex8,
    },
    ShiftRightCR {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: RegisterIndex,
    },
    ShiftRightCC {
        dest: RegisterIndex,
        left: ConstantIndex8,
        right: ConstantIndex8,
    },
    BitNot {
        dest: RegisterIndex,
        source: RegisterIndex,
    },
}
