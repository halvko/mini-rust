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
#[allow(clippy::upper_case_acronyms)]
pub struct LLVM {
    memory_indirection: bool,
}

impl From<&crate::Options> for LLVM {
    fn from(value: &crate::Options) -> Self {
        Self {
            memory_indirection: value.memory_indirection,
        }
    }
}

impl LLVM {
    pub fn store<'a, 'b, 'c>(
        &self,
        ty: impl Into<CowType<'a>>,
        from: impl Into<CowReg<'b>>,
        to: impl Into<CowReg<'c>>,
        o: &mut impl io::Write,
    ) -> anyhow::Result<()> {
        let from = from.into();
        let ty = ty.into();
        let to = to.into();
        let store = Store {
            ty: ty.as_ref(),
            from: from.as_ref(),
            to: to.as_ref(),
        };
        if self.memory_indirection {
            store.indirect(o)
        } else {
            store.direct(o)
        }
    }

    pub fn load<'a, 'b, 'c>(
        &self,
        ty: impl Into<CowType<'a>>,
        to: impl Into<CowReg<'b>>,
        from: impl Into<CowReg<'c>>,
        o: &mut impl io::Write,
    ) -> anyhow::Result<()> {
        let ty = ty.into();
        let from = from.into();
        let to = to.into();
        let load = Load {
            ty: ty.as_ref(),
            from: from.as_ref(),
            to: to.as_ref(),
        };
        if self.memory_indirection {
            load.indirect(o)
        } else {
            load.direct(o)
        }
    }
}

struct Load<'a> {
    ty: TypeRef<'a>,
    from: RegRef<'a>,
    to: RegRef<'a>,
}

impl Load<'_> {
    fn direct(&self, o: &mut impl io::Write) -> anyhow::Result<()> {
        let Self { ty, from, to } = self;
        writeln!(o, "{to} = load {ty}* {from}")?;
        Ok(())
    }
    fn indirect(&self, o: &mut impl io::Write) -> anyhow::Result<()> {
        let Self { ty, from, to } = self;
        writeln!(o, "{to} = call {ty} @load_{ty}(ptr %mm, {ty}* {from})")?;
        Ok(())
    }
}

struct Store<'a> {
    ty: TypeRef<'a>,
    to: RegRef<'a>,
    from: RegRef<'a>,
}

impl Store<'_> {
    fn direct(&self, o: &mut impl io::Write) -> anyhow::Result<()> {
        let Self { ty, to, from } = self;
        writeln!(o, "store {ty} {from}, {ty}* {to}")?;
        Ok(())
    }
    fn indirect(&self, o: &mut impl io::Write) -> anyhow::Result<()> {
        let Self { ty, to, from } = self;
        writeln!(o, "call void @store_{ty}(ptr %mm, {ty} {from}, {ty}* {to})")?;
        Ok(())
    }
}
