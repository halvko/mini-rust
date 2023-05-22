use std::{fmt::Display, io};
mod reg;

pub use r#type::{CowType, Type, TypeRef};
pub use reg::{CowReg, Reg, RegRef};

mod r#type;

pub struct Label {
    name: String,
}

impl Label {
    pub fn from_name(name: String) -> Self {
        Self { name }
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}

pub fn store<'a, 'b, 'c>(
    ty: impl Into<CowType<'a>>,
    from: impl Into<CowReg<'b>>,
    to: impl Into<CowReg<'c>>,
    o: &mut impl io::Write,
) -> anyhow::Result<()> {
    let ty = ty.into();
    let from = from.into();
    let to = to.into();
    writeln!(o, "call void @store_{ty}(ptr %mm, {ty} {from}, {ty}* {to})")?;
    Ok(())
}

pub fn load<'a, 'b, 'c>(
    ty: impl Into<CowType<'a>>,
    to: impl Into<CowReg<'b>>,
    from: impl Into<CowReg<'c>>,
    o: &mut impl io::Write,
) -> anyhow::Result<()> {
    let ty = ty.into();
    let from = from.into();
    let to = to.into();
    writeln!(o, "{to} = call {ty} @load_{ty}(ptr %mm, {ty}* {from})")?;
    Ok(())
}
