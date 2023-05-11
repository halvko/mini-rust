use std::{collections::HashMap, hash::Hash};

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
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
