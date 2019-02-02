use super::grammar::{ContextFreeGrammar, Production};
use super::language::*;

use std::collections::{HashMap, HashSet};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum VariableType {
    Prog,
    ClassDecl,
    FuncDecl,
    FuncHead,
    FuncDef,
    FuncBody,
    VarDecl,
    Statement,
    AssignStat,
    StatBlock,
    Expr,
    RelExpr,
    ArithExpr,
    Sign,
    Term,
    Factor,
    Variable,
    FunctionCall,
    Idnest,
    Indice,
    ArraySize,
    Type,
    FParam,
    AParam,
    FParamTail,
    AParamTail,
    AssignOp,
    RelOp,
    AndOp,
    MultOp,
}

pub fn define_grammar() -> ContextFreeGrammar<VariableType, TokenType> {
    //use Symbol::*;
    use TokenType::*;
    use VariableType::*;

    let mut variables: HashSet<VariableType> = HashSet::new();
    let mut terminals: HashSet<TokenType> = HashSet::new();
    let start = Prog;
    let mut productions: Vec<Production<VariableType, TokenType>> = Vec::new();

    // productions.push(Production {
    //     lhs: Expression,
    //     rhs: vec![
    //         Variable(Expression),
    //         Terminal(Some(Plus)),
    //         Variable(Expression),
    //     ],
    // });

    let grammar = ContextFreeGrammar {
        variables,
        terminals,
        start,
        productions,
    };

    grammar
}
