use std::{collections::HashMap, hash::Hash};

#[derive(Debug)]
pub struct SymbolTable<T: Eq + Hash + Clone> {
    to_original: HashMap<Symbol, T>,
    to_symbol: HashMap<T, Symbol>,
}

impl<T: Eq + Hash + Clone> Default for SymbolTable<T> {
    fn default() -> Self {
        Self {
            to_original: HashMap::new(),
            to_symbol: HashMap::new(),
        }
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct Symbol(usize);

impl<T: Eq + Hash + Clone> SymbolTable<T> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn symbol(&mut self, t: T) -> Symbol {
        if let Some(s) = self.to_symbol.get(&t) {
            return *s;
        }
        let symbol = loop {
            let symbol = Symbol(rand::random());
            if !self.to_original.contains_key(&symbol) {
                break symbol;
            }
        };
        self.to_original.insert(symbol, t.clone());
        self.to_symbol.insert(t, symbol);
        symbol
    }

    pub fn original(&self, s: Symbol) -> &T {
        self.to_original.get(&s).unwrap()
    }
}

impl<T: Eq + Hash + Clone + std::fmt::Display> SymbolTable<T> {
    pub fn displayable<'a, U: STDisplay>(&'a self, u: &'a U) -> Displayable<'a, U, T> {
        Displayable { st: self, t: u }
    }
}

pub trait STDisplay {
    fn fmt<SymbolTableType: Eq + Clone + Hash + std::fmt::Display>(
        &self,
        f: &mut std::fmt::Formatter,
        st: &SymbolTable<SymbolTableType>,
    ) -> std::fmt::Result;
}

pub struct Displayable<'a, T, SymbolTableType>
where
    T: STDisplay,
    SymbolTableType: Eq + Clone + Hash + std::fmt::Display,
{
    st: &'a SymbolTable<SymbolTableType>,
    t: &'a T,
}

impl<T: STDisplay, SymbolTableType: Eq + Clone + Hash + std::fmt::Display> std::fmt::Display
    for Displayable<'_, T, SymbolTableType>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.t.fmt(f, self.st)
    }
}
