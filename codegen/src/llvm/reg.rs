use std::fmt::Display;

pub struct Reg {
    name: String,
}

impl Reg {
    pub fn from_plain(s: &str) -> Self {
        Reg {
            name: format!("%{s}"),
        }
    }

    pub fn from_reg_string(reg: String) -> Self {
        Reg { name: reg }
    }

    pub fn as_ref(&self) -> RegRef {
        RegRef { name: &self.name }
    }

    pub fn zero() -> RegRef<'static> {
        RegRef { name: "0" }
    }

    pub fn from_value(v: typecheck::Value) -> CowReg<'static> {
        match v {
            typecheck::Value::USize(u) => CowReg::Owned(Reg {
                name: u.to_string(),
            }),
            typecheck::Value::ISize(i) => CowReg::Owned(Reg {
                name: i.to_string(),
            }),
            typecheck::Value::Bool(b) => CowReg::Borrowed(RegRef {
                name: match b {
                    true => "1",
                    false => "0",
                },
            }),
        }
    }
}

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

#[derive(Clone, Copy)]
pub struct RegRef<'a> {
    name: &'a str,
}

impl RegRef<'_> {
    fn to_owned(&self) -> Reg {
        Reg {
            name: self.name.to_owned(),
        }
    }
}

impl Display for RegRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

pub enum CowReg<'a> {
    Owned(Reg),
    Borrowed(RegRef<'a>),
}

impl From<Reg> for CowReg<'static> {
    fn from(value: Reg) -> Self {
        CowReg::Owned(value)
    }
}

impl<'a> From<RegRef<'a>> for CowReg<'a> {
    fn from(value: RegRef<'a>) -> Self {
        Self::Borrowed(value)
    }
}

impl CowReg<'_> {
    pub fn as_ref<'a>(&'a self) -> RegRef<'a> {
        match self {
            CowReg::Owned(reg) => reg.as_ref(),
            CowReg::Borrowed(b) => *b,
        }
    }
}

impl Display for CowReg<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_ref().fmt(f)
    }
}
