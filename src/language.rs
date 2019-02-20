use super::tree::{Tree, Node};

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
    ClassDeclList,
    FuncDecl,
    FuncHead,
    ScopeOp,
    FuncDef,
    FuncDefList,
    FuncBody,
    MemberDecl,
    MemberDeclPrime,
    MemberList,
    NumVarDecl,
    AssignStat,
    Statement,
    StatOrAssign,
    StatOrAssignList,
    StatOrAssignOrVarDecl,
    StatOrAssignOrVarDeclPrime,
    StatOrAssignOrVarDeclList,
    StatBlock,
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
    InherListOp,
    InherList,
    Index,
    IndexList,
    Dim,
    DimList,
    Type,
    FParam,
    FParamList,
    FParamListPrime,
    AParam,
    AParamList,
    AParamListPrime,
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
            "classDeclList" => Ok(ClassDeclList),
            "funcDecl" => Ok(FuncDecl),
            "funcHead" => Ok(FuncHead),
            "scopeOp" => Ok(ScopeOp),
            "funcDef" => Ok(FuncDef),
            "funcDefList" => Ok(FuncDefList),
            "funcBody" => Ok(FuncBody),
            "memberDecl" => Ok(MemberDecl),
            "memberDecl'" => Ok(MemberDeclPrime),
            "memberList" => Ok(MemberList),
            "numVarDecl" => Ok(NumVarDecl),
            "assignStat" => Ok(AssignStat),
            "statement" => Ok(Statement),
            "statOrAssign" => Ok(StatOrAssign),
            "statOrAssignList" => Ok(StatOrAssignList),
            "statOrAssignOrVarDecl" => Ok(StatOrAssignOrVarDecl),
            "statOrAssignOrVarDecl'" => Ok(StatOrAssignOrVarDeclPrime),
            "statOrAssignOrVarDeclList" => Ok(StatOrAssignOrVarDeclList),
            "statBlock" => Ok(StatBlock),
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
            "inherListOp" => Ok(InherListOp),
            "inherList" => Ok(InherList),
            "index" => Ok(Index),
            "indexList" => Ok(IndexList),
            "dim" => Ok(Dim),
            "dimList" => Ok(DimList),
            "type" => Ok(Type),
            "fParam" => Ok(FParam),
            "fParamList" => Ok(FParamList),
            "fParamList'" => Ok(FParamListPrime),
            "aParam" => Ok(AParam),
            "aParamList" => Ok(AParamList),
            "aParamList'" => Ok(AParamListPrime),
            "assignOp" => Ok(AssignOp),
            "relOp" => Ok(RelOp),
            "addOp" => Ok(AddOp),
            "multOp" => Ok(MultOp),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum SemanticActionType {
    MakeNodeId,
    MakeNodeNum,
    MakeNodeRelOp,
    MakeNodeType,
    MakeNodeClassDeclList,
    MakeNodeFuncDefList,
    MakeNodeProg,
    MakeNodeMemberList,
    MakeNodeClassDecl,
    MakeNodeFuncDecl,
    MakeNodeFuncDef,
    MakeNodeStatBlock,
    MakeNodeDimList,
    MakeNodeVarDecl,
    MakeNodeAssignStat,
    MakeNodeIfStat,
    MakeNodeForStat,
    MakeNodeReadStat,
    MakeNodeWriteStat,
    MakeNodeReturnStat,
    MakeNodeIndexList,
    MakeNodeRelExpr,
    MakeNodeAddOp,
    MakeNodeMultOp,
    MakeNodeNot,
    MakeNodeSign,
    MakeNodeFunctionCall,
    MakeNodeInheritList,
    MakeNodeFParam,
    MakeNodeFParamList,
    MakeNodeAParamList,
}

impl FromStr for SemanticActionType {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        use SemanticActionType::*;
        match s {
            "#MakeNodeId" => Ok(MakeNodeId),
            "#MakeNodeNum" => Ok(MakeNodeNum),
            "#MakeNodeRelOp" => Ok(MakeNodeRelOp),
            "#MakeNodeType" => Ok(MakeNodeType),
            "#MakeNodeClassDeclList" => Ok(MakeNodeClassDeclList),
            "#MakeNodeFuncDefList" => Ok(MakeNodeFuncDefList),
            "#MakeNodeProg" => Ok(MakeNodeProg),
            "#MakeNodeMemberList" => Ok(MakeNodeMemberList),
            "#MakeNodeClassDecl" => Ok(MakeNodeClassDecl),
            "#MakeNodeFuncDecl" => Ok(MakeNodeFuncDecl),
            "#MakeNodeFuncDef" => Ok(MakeNodeFuncDef),
            "#MakeNodeStatBlock" => Ok(MakeNodeStatBlock),
            "#MakeNodeDimList" => Ok(MakeNodeDimList),
            "#MakeNodeVarDecl" => Ok(MakeNodeVarDecl),
            "#MakeNodeAssignStat" => Ok(MakeNodeAssignStat),
            "#MakeNodeIfStat" => Ok(MakeNodeIfStat),
            "#MakeNodeForStat" => Ok(MakeNodeForStat),
            "#MakeNodeReadStat" => Ok(MakeNodeReadStat),
            "#MakeNodeWriteStat" => Ok(MakeNodeWriteStat),
            "#MakeNodeReturnStat" => Ok(MakeNodeReturnStat),
            "#MakeNodeIndexList" => Ok(MakeNodeIndexList),
            "#MakeNodeRelExpr" => Ok(MakeNodeRelExpr),
            "#MakeNodeAddOp" => Ok(MakeNodeAddOp),
            "#MakeNodeMultOp" => Ok(MakeNodeMultOp),
            "#MakeNodeNot" => Ok(MakeNodeNot),
            "#MakeNodeSign" => Ok(MakeNodeSign),
            "#MakeNodeFunctionCall" => Ok(MakeNodeFunctionCall),
            "#MakeNodeInheritList" => Ok(MakeNodeInheritList),
            "#MakeNodeFParam" => Ok(MakeNodeFParam),
            "#MakeNodeFParamList" => Ok(MakeNodeFParamList),
            "#MakeNodeAParamList" => Ok(MakeNodeAParamList),
            _ => Err(()),
        }
    }
}

struct NodeElement {
    node_type: SemanticActionType,
}

impl Tree<NodeElement> {
    fn make_node(&mut self, semantic_stack: &mut Vec<Node<NodeElement>>, semantic_action: SemanticActionType) {
        match semantic_action {
            MakeNodeId => {
                let node = Node {
                    index : 0,
                    parent: None,
                    children: Vec::new(),
                    element: NodeElement {
                        node_type: semantic_action
                    }
                };
                semantic_stack.push(node);
            }
            MakeNodeNum => {
                let node = Node {
                    index : 0,
                    parent: None,
                    children: Vec::new(),
                    element: NodeElement {
                        node_type: semantic_action
                    }
                };
                semantic_stack.push(node);
            }
            MakeNodeRelOp => {
                let node = Node {
                    index : 0,
                    parent: None,
                    children: Vec::new(),
                    element: NodeElement {
                        node_type: semantic_action
                    }
                };
                semantic_stack.push(node);
            }
            MakeNodeType => {
                let node = Node {
                    index : 0,
                    parent: None,
                    children: Vec::new(),
                    element: NodeElement {
                        node_type: semantic_action
                    }
                };
                semantic_stack.push(node);
            }
            MakeNodeClassDeclList => {}
            MakeNodeFuncDefList => {}
            MakeNodeProg => {}
            MakeNodeMemberList => {}
            MakeNodeClassDecl => {}
            MakeNodeFuncDecl => {}
            MakeNodeFuncDef => {}
            MakeNodeStatBlock => {}
            MakeNodeDimList => {}
            MakeNodeVarDecl => {
                let dim_list_node = semantic_stack.pop().unwrap();
                let id_node = semantic_stack.pop().unwrap();
                let type_node = semantic_stack.pop().unwrap();
                let node = Node {
                    index : 0,
                    parent: None,
                    children: Vec::new(),
                    element: NodeElement {
                        node_type: semantic_action
                    }
                };
                semantic_stack.push(node);
            }
            MakeNodeAssignStat => {}
            MakeNodeIfStat => {}
            MakeNodeForStat => {}
            MakeNodeReadStat => {}
            MakeNodeWriteStat => {}
            MakeNodeReturnStat => {}
            MakeNodeIndexList => {}
            MakeNodeRelExpr => {}
            MakeNodeAddOp => {}
            MakeNodeMultOp => {}
            MakeNodeNot => {}
            MakeNodeSign => {}
            MakeNodeFunctionCall => {}
            MakeNodeInheritList => {}
            MakeNodeFParam => {}
            MakeNodeFParamList => {}
            MakeNodeAParamList => {}

        }
    }
}
