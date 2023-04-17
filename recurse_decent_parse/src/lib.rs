fn main() {
    println!("Hello, world!");
}

struct Program {
    functions: Vec<Function>
}

struct Function {
    name: String,
    args: Vec<TypedVar>,
    return_type: Type,
    block: Expression,
}

struct TypedVar {
    name: String,
    r#type: Type,
}

struct Type {
    name: String,
}

struct Expression {

}

enum Const {
    Int {
        value:
    }
}

struct LetStmt {
    var_name: String,
    mutable: bool,
    assign_exp: Exp,
}

fn parse_let() {}

fn parse_const()