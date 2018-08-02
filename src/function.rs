use opcode::OpCode;
use value::Value;

#[derive(Collect)]
#[collect(empty_drop)]
pub struct FunctionProto<'gc> {
    pub fixed_params: u8,
    pub constants: Vec<Value<'gc>>,
    pub opcodes: Vec<OpCode>,
}
