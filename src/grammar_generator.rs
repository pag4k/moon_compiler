use super::grammar::{ContextFreeGrammar, Production, Symbol};
use super::language::{CommentType, KeywordType, OperatorType, SeparatorType, TokenType};

use std::collections::{HashMap, HashSet};
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum VariableType {
    Prog,
    ClassDecl,
    ClassDeclRep,
    FuncDecl,
    FuncHead,
    FuncScopeOp,
    FuncDef,
    FuncDefRep,
    FuncBody,
    VarDecl,
    VarDeclRep,
    Decl,
    DeclPrime,
    DeclRep,
    NumVarDecl,
    BodyElement,
    BodyElementPrime,
    BodyElementRep,
    Statement,
    StatementRep,
    AssignStat,
    StatBlock,
    ScopeOp,
    Expr,
    ExprPrime,
    RelExpr,
    ArithExpr,
    ArithExprPrime,
    Sign,
    Term,
    TermPrime,
    Factor,
    FactorPrime,
    Variable,
    FunctionCall,
    Idnest,
    IdnestPrime,
    IdnestRep,
    ParentOp,
    ParentListRep,
    Indice,
    IndiceRep,
    ArraySize,
    ArraySizeRep,
    Type,
    FParams,
    AParams,
    FParamsTail,
    FParamsTailRep,
    AParamsTail,
    AParamsTailRep,
    AssignOp,
    RelOp,
    AddOp,
    MultOp,
}

impl FromStr for VariableType {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        use VariableType::*;
        match s {
            "prog" => Ok(Prog),
            "classDecl" => Ok(ClassDecl),
            "classDeclRep" => Ok(ClassDeclRep),
            "funcDecl" => Ok(FuncDecl),
            "funcHead" => Ok(FuncHead),
            "funcScopeOp" => Ok(FuncScopeOp),
            "funcDef" => Ok(FuncDef),
            "funcDefRep" => Ok(FuncDefRep),
            "funcBody" => Ok(FuncBody),
            "varDecl" => Ok(VarDecl),
            "varDeclRep" => Ok(VarDeclRep),
            "decl" => Ok(Decl),
            "decl'" => Ok(DeclPrime),
            "declRep" => Ok(DeclRep),
            "numVarDecl" => Ok(NumVarDecl),
            "bodyElement" => Ok(BodyElement),
            "bodyElement'" => Ok(BodyElementPrime),
            "bodyElementRep" => Ok(BodyElementRep),
            "statement" => Ok(Statement),
            "statementRep" => Ok(StatementRep),
            "assignStat" => Ok(AssignStat),
            "statBlock" => Ok(StatBlock),
            "scopeOp" => Ok(ScopeOp),
            "expr" => Ok(Expr),
            "expr'" => Ok(ExprPrime),
            "relExpr" => Ok(RelExpr),
            "arithExpr" => Ok(ArithExpr),
            "arithExpr'" => Ok(ArithExprPrime),
            "sign" => Ok(Sign),
            "term" => Ok(Term),
            "term'" => Ok(TermPrime),
            "factor" => Ok(Factor),
            "factor'" => Ok(FactorPrime),
            "variable" => Ok(Variable),
            "functionCall" => Ok(FunctionCall),
            "idnest" => Ok(Idnest),
            "idnest'" => Ok(IdnestPrime),
            "idnestRep" => Ok(IdnestRep),
            "parentOp" => Ok(ParentOp),
            "parentListRep" => Ok(ParentListRep),
            "indice" => Ok(Indice),
            "indiceRep" => Ok(IndiceRep),
            "arraySize" => Ok(ArraySize),
            "arraySizeRep" => Ok(ArraySizeRep),
            "type" => Ok(Type),
            "fParams" => Ok(FParams),
            "aParams" => Ok(AParams),
            "fParamsTail" => Ok(FParamsTail),
            "fParamsTailRep" => Ok(FParamsTailRep),
            "aParamsTail" => Ok(AParamsTail),
            "aParamsTailRep" => Ok(AParamsTailRep),
            "assignOp" => Ok(AssignOp),
            "relOp" => Ok(RelOp),
            "addOp" => Ok(AddOp),
            "multOp" => Ok(MultOp),
            _ => Err(()),
        }
    }
}

pub fn define_grammar() -> ContextFreeGrammar<VariableType, TokenType> {
    use KeywordType::*;
    use OperatorType::*;
    use SeparatorType::*;
    use Symbol::*;
    use TokenType::*;
    use VariableType::*;

    let mut variables: HashSet<VariableType> = HashSet::new();
    let mut terminals: HashSet<TokenType> = HashSet::new();
    let start = Prog;
    let mut productions: Vec<Production<VariableType, TokenType>> = Vec::new();

    let g = KeywordType::Main;

    productions.push(Production {
        lhs: Prog,
        rhs: vec![
            NonTerminal(ClassDeclRep),
            NonTerminal(FuncDefRep),
            Terminal(Some(Keyword(Main))),
            Terminal(Some(Separator(SemiColon))),
        ],
    });

    productions.push(Production {
        lhs: ClassDecl,
        rhs: vec![
            Terminal(Some(Keyword(Class))),
            Terminal(Some(Id)),
            Terminal(Some(Separator(LeftSquareBracket))),
            NonTerminal(ClassDeclRep),
            NonTerminal(FuncDefRep),
            Terminal(Some(Separator(SemiColon))),
        ],
    });

    let grammar = ContextFreeGrammar {
        variables,
        terminals,
        start,
        productions,
    };

    grammar
}
