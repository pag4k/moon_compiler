use std::fmt::{Display, Formatter};
use std::str::FromStr;

// Define the all the arrays of char describing the language.
// Note that the first char is used as an identifier of the array.
pub const SIGMA: [char; 87] = [
    'Σ', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',
    'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A',
    'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
    'U', 'V', 'W', 'X', 'Y', 'Z', '!', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=',
    '>', '[', ']', '_', '{', '}', '&', '|', ' ', '\t', '\n',
];
pub const NONZERO: [char; 10] = ['N', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
pub const DIGIT: [char; 11] = ['D', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
pub const LETTER: [char; 53] = [
    'L', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
    's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K',
    'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
];

//#[derive(Clone, Copy)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TokenType {
    Id,
    IntNum,
    FloatNum,
    Keyword(KeywordType),
    Operator(OperatorType),
    Separator(SeparatorType),
    Comment(CommentType),
    LexicalError(LexicalError),
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use TokenType::*;
        match self {
            Id => write!(f, "id"),
            IntNum => write!(f, "intNum"),
            FloatNum => write!(f, "floatNum"),
            Keyword(keyword) => write!(f, "{:?}", keyword),
            Operator(operator) => write!(f, "{:?}", operator),
            Separator(separator) => write!(f, "{:?}", separator),
            Comment(comment) => write!(f, "{:?}", comment),
            LexicalError(error) => write!(f, "{}", error),
        }
    }
}

impl FromStr for TokenType {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        use KeywordType::*;
        use OperatorType::*;
        use SeparatorType::*;
        use TokenType::*;
        match s {
            "'id'" => Ok(Id),
            "'intNum'" => Ok(IntNum),
            "'floatNum'" => Ok(FloatNum),
            "'if'" => Ok(Keyword(If)),
            "'then'" => Ok(Keyword(Then)),
            "'else'" => Ok(Keyword(Else)),
            "'for'" => Ok(Keyword(For)),
            "'class'" => Ok(Keyword(Class)),
            "'integer'" => Ok(Keyword(Integer)),
            "'float'" => Ok(Keyword(Float)),
            "'read'" => Ok(Keyword(Read)),
            "'write'" => Ok(Keyword(Write)),
            "'return'" => Ok(Keyword(Return)),
            "'main'" => Ok(Keyword(Main)),
            "'lt'" => Ok(Operator(LT)),
            "'leq'" => Ok(Operator(LEq)),
            "'neq'" => Ok(Operator(NEq)),
            "'gt'" => Ok(Operator(GT)),
            "'geq'" => Ok(Operator(GEq)),
            "'='" => Ok(Operator(Assignment)),
            "'eq'" => Ok(Operator(Eq)),
            "'+'" => Ok(Operator(Addition)),
            "'-'" => Ok(Operator(Subtraction)),
            "'*'" => Ok(Operator(Multiplication)),
            "'/'" => Ok(Operator(Division)),
            "'and'" => Ok(Operator(And)),
            "'not'" => Ok(Operator(Not)),
            "'or'" => Ok(Operator(Or)),
            "'sr'" => Ok(Operator(SR)),
            "';'" => Ok(Separator(SemiColon)),
            "','" => Ok(Separator(Coma)),
            "'.'" => Ok(Separator(Period)),
            "':'" => Ok(Separator(Colon)),
            "'('" => Ok(Separator(LeftParenthesis)),
            "')'" => Ok(Separator(RightParenthesis)),
            "'{'" => Ok(Separator(LeftCurlyBracket)),
            "'}'" => Ok(Separator(RightCurlyBracket)),
            "'['" => Ok(Separator(LeftSquareBracket)),
            "']'" => Ok(Separator(RightSquareBracket)),
            _ => Err(()),
        }
    }
}

//#[derive(Debug, Clone, Copy)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum KeywordType {
    If,
    Then,
    Else,
    For,
    Class,
    Integer,
    Float,
    Read,
    Write,
    Return,
    Main,
}

impl FromStr for KeywordType {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        use KeywordType::*;
        match s {
            "if" => Ok(If),
            "then" => Ok(Then),
            "else" => Ok(Else),
            "for" => Ok(For),
            "class" => Ok(Class),
            "integer" => Ok(Integer),
            "float" => Ok(Float),
            "read" => Ok(Read),
            "write" => Ok(Write),
            "return" => Ok(Return),
            "main" => Ok(Main),
            _ => Err(()),
        }
    }
}

//#[derive(Debug, Clone, Copy)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum OperatorType {
    LT,
    LEq,
    NEq,
    GT,
    GEq,
    Assignment,
    Eq,
    Addition,
    Subtraction,
    Multiplication,
    Division,
    And,
    Not,
    Or,
    SR,
}

//#[derive(Debug, Clone, Copy)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum SeparatorType {
    SemiColon,
    Coma,
    Period,
    Colon,
    LeftParenthesis,
    RightParenthesis,
    LeftCurlyBracket,
    RightCurlyBracket,
    LeftSquareBracket,
    RightSquareBracket,
}

//#[derive(Debug, Clone, Copy)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum CommentType {
    BlockComment,
    LineComment,
}

//#[derive(Debug, Clone, Copy)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum LexicalError {
    InvalidCharacter,
    InvalidToken,
    InvalidId,
    FloatMissingFraction,
    FloatTrailingZeros,
    FloatMissingExponent,
    IncompleteAnd,
    IncompleteOr,
    UnterminatedBlockComment,
}

impl Display for LexicalError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            LexicalError::InvalidCharacter => write!(f, "Lexical Error: Invalid Character"),
            LexicalError::InvalidToken => write!(f, "Lexical Error: Invalid Token"),
            LexicalError::InvalidId => write!(f, "Lexical Error: Invalid Id"),
            LexicalError::FloatMissingFraction => {
                write!(f, "Lexical Error: Float Missing Fraction")
            }
            LexicalError::FloatTrailingZeros => write!(f, "Lexical Error: Float Trailing Zeros"),
            LexicalError::FloatMissingExponent => {
                write!(f, "Lexical Error: Float Missing Exponent")
            }
            LexicalError::IncompleteAnd => {
                write!(f, "Lexical Error: Incomplete And (did you mean '&&'?)")
            }
            LexicalError::IncompleteOr => {
                write!(f, "Lexical Error: Incomplete Or (did you mean '||'?)")
            }
            LexicalError::UnterminatedBlockComment => write!(
                f,
                "Lexical Error: Unterminated Block Comment (you are missing '*/')"
            ),
        }
    }
}

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
