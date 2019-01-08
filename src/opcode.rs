use gc_arena::Collect;

use crate::types::{
    ConstantIndex16, ConstantIndex8, Opt254, PrototypeIndex, RegisterIndex, UpValueIndex, VarCount,
};

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_static)]
pub enum OpCode {
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
    Call {
        func: RegisterIndex,
        args: VarCount,
        returns: VarCount,
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
        // If set, close upvalues >= `close_upvalues`
        close_upvalues: Opt254,
    },
    // Test the register as a boolean, if its boolean value matches `is_true`, skip the next
    // instruction.
    Test {
        value: RegisterIndex,
        is_true: bool,
    },
    // Test the value at the `value` register as a boolean, if its boolean value matches `is_true`,
    // skip the next instruction, otherwise assign the given value (not converted to a boolean) to
    // the destination register.
    TestSet {
        dest: RegisterIndex,
        value: RegisterIndex,
        is_true: bool,
    },
    Closure {
        dest: RegisterIndex,
        proto: PrototypeIndex,
    },
    // Used to set up for a generic for loop.
    // R(base + 3), ..., R(base + 2 + var_count) = R(base)(R(base + 1), R(base + 2))
    ForCall {
        base: RegisterIndex,
        var_count: u8,
    },
    // Used to iterate a for loop:
    // if R(base + 1) ~= nil then
    //     R(base) = R(base + 1)
    //     pc += jump
    // end
    ForLoop {
        base: RegisterIndex,
        jump: i16,
    },
    GetUpValue {
        dest: RegisterIndex,
        source: UpValueIndex,
    },
    SetUpValue {
        dest: UpValueIndex,
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
    Not {
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
}
