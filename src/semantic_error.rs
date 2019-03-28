use crate::lexical_analyzer::*;
use crate::symbol_table::*;

use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum SemanticError {
    //Class errors
    CircularClassDependency(Token, Vec<String>),
    DotOperatorWithInvalidClass(Token, SymbolType),
    //Member function errors
    MismatchMemberFunctionDeclAndDef(Token, String, SymbolKind, SymbolKind),
    MemberFunctionDefDoesNotHaveDecl(Token, String),
    MemberFunctionDeclHasNoDef(Token, String),
    //Identifier errors
    DuplicateIdentifier(Token, String, String),
    VariableUsedBeforeBeingDeclared(Token, String),
    ForVariableUsedOutOfScope(Token, String),
    UndefinedLocalVariable(Token, String),
    UndefinedFunction(Token),
    //UndefinedFreeFunction(Token),
    UndefinedMemberVariable(Token, SymbolType),
    UndefinedMemberFunction(Token, SymbolType),
    UndefinedClass(Token),
    MismatchedNumberOfDimension(Token, usize, usize),
    //Type error
    InvalidAddOperation(Token, SymbolType, SymbolType),
    InvalidMultOperation(Token, SymbolType, SymbolType),
    InvalidRelOperation(Token, SymbolType, SymbolType),
    MismatchedTypes(Token, SymbolType, SymbolType),
    ShouldNotReturnFromMain(Token),
    ReturnTypeDoesNotMatchFuctionDeclaration(Token, SymbolType, SymbolType),
    ArrayIndiceMustBeInteger(Token, (usize, SymbolType)),
    DimensionMustBeGreaterThanZero(Token, usize, usize),
    MismatchedTypeDimensions(Token, SymbolType, SymbolType),
    WrongNumberOfArguments(Token, usize, usize),
    InvalidArgument(Token, (usize, SymbolType, SymbolType)),
    // Warnings
    ShadowInheritedMemberVariable(Token, String, String, String),
    ShadowInheritedMemberFunction(Token, String, String, String),
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use SemanticError::*;
        match self {
            CircularClassDependency(token, class_name_list) => {
                let mut output = String::new();
                for class_name in class_name_list.iter() {
                    output.push_str(&format!("{}-", class_name));
                }
                output.pop();
                write!(
                    f,
                    "Semantic error at {}: Class circular dependency: {}.",
                    token.location,
                    output
                    
                )
            }
            DotOperatorWithInvalidClass(token, symbol_type) => write!(
                f,
                "Semantic error at {}: Invalid dot operator since type \"{}\" is not a valid class.",
                token.location,
                symbol_type
                
            ),
            MismatchMemberFunctionDeclAndDef(token,function_name, declaration_kind, definition_kind) => write!(
                f,
                "Semantic error at {}: Member function \"{}\" definition does not match its declaration: expected {}, but found {}.",
                token.location,
                function_name,
                declaration_kind,
                definition_kind,
            ),
            MemberFunctionDefDoesNotHaveDecl(token, function_name) => write!(
                f,
                "Semantic error at {}: Member function \"{}\" does not have a declation.",
                token.location,
                function_name
            ),
            MemberFunctionDeclHasNoDef(token, function_name) => write!(
                f,
                "Semantic error at {}: Member function \"{}\" does not have a definition.",
                token.location,
                function_name
            ),
            DuplicateIdentifier(token, scope_name, identifier_name) => write!(
                f,
                "Semantic error at {}: Duplicate identifier in scope \"{}\": \"{}\".",
                token.location,
                scope_name,
                identifier_name,
            ),
            VariableUsedBeforeBeingDeclared(token, scope_name) => write!(
                f,
                "Semantic error at {}: Varible \"{}\" is used before being declared in scope \"{}\".",
                token.location,
                token.lexeme.clone().unwrap(),
                scope_name,
            ),
            ForVariableUsedOutOfScope(token, scope_name) => write!(
                f,
                "Semantic error at {}: For varible \"{}\" in \"{}\" is used out of the loop scope.",
                token.location,
                token.lexeme.clone().unwrap(),
                scope_name,
            ),
            UndefinedLocalVariable(token, scope_name) => write!(
                f,
                "Semantic error at {}: Variable \"{}\" is not defined in scope \"{}\".",
                token.location,
                token.lexeme.clone().unwrap(),
                scope_name,
            ),
            UndefinedFunction(token) => write!(
                f,
                "Semantic error at {}: Function \"{}\" is not defined.",
                token.location,
                token.lexeme.clone().unwrap(),
            ),
            // UndefinedFreeFunction(token) => write!(
            //     f,
            //     "Semantic error at {}: Free function \"{}\" is not defined.",
            //     token.location,
            //     token.lexeme.clone().unwrap(),
            // ),
            UndefinedMemberVariable(token, class_type) => write!(
                f,
                "Semantic error at {}: Variable \"{}\" is not a member of class {}.",
                token.location,
                token.lexeme.clone().unwrap(),
                class_type,
            ),
            UndefinedMemberFunction(token, class_type) => write!(
                f,
                "Semantic error at {}: Function \"{}\" is not a member of class {}.",
                token.location,
                token.lexeme.clone().unwrap(),
                class_type,
            ),
            UndefinedClass(token) => write!(
                f,
                "Semantic error at {}: Undefined class: \"{}\".",
                token.location,
                token.lexeme.clone().unwrap(),
            ),
            MismatchedNumberOfDimension(token, expected_number, actual_number) => write!(
                f,
                "Semantic error at {}: Variable \"{}\" is expecting {} dimensions, but found {}.",
                token.location,
                token.lexeme.clone().unwrap(),
                expected_number,
                actual_number,
            ),
            InvalidAddOperation(token, expected_type, actual_type) => write!(
                f,
                "Semantic error at {}: Invalid add operation with types {} and {}.",
                token.location,
                expected_type,
                actual_type,
            ),
            InvalidMultOperation(token, expected_type, actual_type) => write!(
                f,
                "Semantic error at {}: Invalid mutliply operation with types {} and {}.",
                token.location,
                expected_type,
                actual_type,
            ),
            InvalidRelOperation(token, expected_type, actual_type) => write!(
                f,
                "Semantic error at {}: Invalid relational operation with types {} and {}.",
                token.location,
                expected_type,
                actual_type,
            ),
            MismatchedTypes(token, expected_type, actual_type) => write!(
                f,
                "Semantic error at {}: Mismatched type with variable \"{}\": expected type {}, but found {}.",
                token.location,
                token.lexeme.clone().unwrap(),
                expected_type,
                actual_type,
            ),
            ShouldNotReturnFromMain(token) => write!(
                f,
                "Semantic error at {}: The main function should not have a return statement.",
                token.location
            ),
            ReturnTypeDoesNotMatchFuctionDeclaration(token, expected_type, actual_type) => write!(
                f,
                "Semantic error at {}: Return type is invalid: expected type {}, but found {}.",
                token.location,
                expected_type,
                actual_type,
            ),
            ArrayIndiceMustBeInteger(token, invalid_index) => write!(
                f,
                "Semantic error at {}: Variable \"{}\" index at position {} must be integer, but found {}.",
                token.location,
                token.lexeme.clone().unwrap(),
                invalid_index.0,
                invalid_index.1,
            ),
            DimensionMustBeGreaterThanZero(token, index, dimension) => write!(
                f,
                "Semantic error at {}: Variable \"{}\" index at position {} must be greater than zero, but found {}.",
                token.location,
                token.lexeme.clone().unwrap(),
                index,
                dimension,
            ),
            MismatchedTypeDimensions(token, expected_type, actual_type) => write!(
                f,
                "Semantic error at {}: Cannot assign to variable \"{}\", dimensions do not match: expected type {}, but found {}.",
                token.location,
                token.lexeme.clone().unwrap(),
                expected_type,
                actual_type,
            ),
            WrongNumberOfArguments(token, expected_number, actual_number) => write!(
                f,
                "Semantic error at {}: Function \"{}\" is expecting {} arguments, but found {}.",
                token.location,
                token.lexeme.clone().unwrap(),
                expected_number,
                actual_number,
            ),
            InvalidArgument(token, invalid_argument) => write!(
                f,
                "Semantic error at {}: In function \"{}\", argument at position {} has invalid type: expecting {}, but found {}.",
                token.location,
                token.lexeme.clone().unwrap(),
                invalid_argument.0,
                invalid_argument.1,
                invalid_argument.2,
            ),
             ShadowInheritedMemberVariable(token, base_class_name, member_name, class_name) => write!(
                f,
                "Semantic warning at {}: Member variable \"{}\" in class \"{}\" shadows the one in class \"{}\".",
                token.location,
                member_name,
                base_class_name,
                class_name,
                
            ),
            ShadowInheritedMemberFunction(token, base_class_name, member_name, class_name) =>write!(
                f,
                "Semantic warning at {}: Member function \"{}\" in class \"{}\" shadows the one in class \"{}\".",
                token.location,
                member_name,
                base_class_name,
                class_name,
                
            ),

        }
    }
}

impl TokenLocation for SemanticError {
    fn get_location(&self) -> Location {
      use SemanticError::*;
        match self {
            CircularClassDependency(token, _) => token.location,
            DotOperatorWithInvalidClass(token, _) => token.location,
            MismatchMemberFunctionDeclAndDef(token,_, _, _) => token.location,
            MemberFunctionDefDoesNotHaveDecl(token, _) => token.location,
            MemberFunctionDeclHasNoDef(token, _) => token.location,
            DuplicateIdentifier(token, _, _) => token.location,
            VariableUsedBeforeBeingDeclared(token, _) => token.location,
            ForVariableUsedOutOfScope(token, _) => token.location,
            UndefinedLocalVariable(token, _) => token.location,
            UndefinedFunction(token) => token.location,
            // UndefinedFreeFunction(token) => token.location,
            UndefinedMemberVariable(token, _) => token.location,
            UndefinedMemberFunction(token, _) => token.location,
            UndefinedClass(token) => token.location,
            MismatchedNumberOfDimension(token, _, _) => token.location,
            InvalidAddOperation(token, _, _) => token.location,
            InvalidMultOperation(token, _, _) => token.location,
            InvalidRelOperation(token, _, _) => token.location,
            MismatchedTypes(token, _, _) => token.location,
            ShouldNotReturnFromMain(token) => token.location,
            ReturnTypeDoesNotMatchFuctionDeclaration(token, _, _) => token.location,
            ArrayIndiceMustBeInteger(token, _) =>token.location,
            DimensionMustBeGreaterThanZero(token, _, _) => token.location,
            MismatchedTypeDimensions(token, _, _) => token.location,
            WrongNumberOfArguments(token, _, _) => token.location,
            InvalidArgument(token, _) => token.location,
            ShadowInheritedMemberVariable(token, _,_,_) => token.location,
            ShadowInheritedMemberFunction(token, _,_,_) => token.location,
        }
    }
}

impl SemanticError {
    pub fn is_warning(&self) -> bool {
      use SemanticError::*;
        match self {
            ShadowInheritedMemberVariable(_, _,_,_) => true,
            ShadowInheritedMemberFunction(_, _,_,_) => true,
            _ => false,
        }
    }
}