use std::fmt::Display;

pub struct Type {
    name: String,
}

impl Type {
    pub fn as_ref(&self) -> TypeRef {
        TypeRef { name: &self.name }
    }
    pub fn from_static(name: &'static str) -> TypeRef<'static> {
        TypeRef { name }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

#[derive(Clone, Copy)]
pub struct TypeRef<'a> {
    name: &'a str,
}

impl TypeRef<'_> {
    pub fn is_void(self) -> bool {
        self.name == "void"
    }

    pub fn is_unsigned(self) -> bool {
        self.name == "usize"
    }
}

impl Display for TypeRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}

pub enum CowType<'a> {
    Owned(Type),
    Borrowed(TypeRef<'a>),
}

impl CowType<'_> {
    pub fn as_ref(&self) -> TypeRef {
        match self {
            CowType::Owned(t) => t.as_ref(),
            CowType::Borrowed(t) => *t,
        }
    }
    pub fn is_void(&self) -> bool {
        self.as_ref().is_void()
    }
    pub fn is_unsigned(&self) -> bool {
        self.as_ref().is_unsigned()
    }
}

impl Display for CowType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_ref().fmt(f)
    }
}

impl From<Type> for CowType<'static> {
    fn from(value: Type) -> Self {
        Self::Owned(value)
    }
}

impl<'a> From<TypeRef<'a>> for CowType<'a> {
    fn from(value: TypeRef<'a>) -> Self {
        Self::Borrowed(value)
    }
}
