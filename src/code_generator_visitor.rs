use crate::ast_node::*;
use crate::ast_visitor::*;
use crate::code_generation_common::*;
use crate::code_generation_error::*;
use crate::language::*;
use crate::memory_table::*;
use crate::symbol_table::*;

use std::collections::HashMap;

pub fn code_generator_visitor(ast: &mut AST) -> Vec<String> {
    use NodeType::*;
    let mut moon_code: Vec<String> = Vec::new();
    let mut semantic_actions: SemanticActionMap<String> = HashMap::new();
    semantic_actions.insert(Prog, prog);
    semantic_actions.insert(MainFuncBody, main_func_body);
    semantic_actions.insert(AssignStat, unary_op);
    semantic_actions.insert(AssignStati, unary_op);
    semantic_actions.insert(VarDecl, var_decl);
    semantic_actions.insert(VarElementList, var_element_list);

    semantic_actions.insert(RelExpr, binary_op);
    semantic_actions.insert(AddOp, binary_op);
    semantic_actions.insert(MultOp, binary_op);
    semantic_actions.insert(Not, unary_op);
    // semantic_actions.insert(Sign, temp_var);
    semantic_actions.insert(Num, num);
    semantic_actions.insert(WriteStat, write);

    ast_traversal(ast, &mut moon_code, &semantic_actions);
    moon_code
}

fn prog(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {}

fn main_func_body(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    // generate moon program's entry point
    moon_code.insert(0, format!("entry"));
    // make the stack frame pointer (address stored in r14) point
    // to the top address allocated to the moon processor
    moon_code.insert(1, format!("addi r14,r0,topaddr"));

    // halting point of the entire program
    moon_code.push(format!("hlt"));

    // DATA SECTION
    // generate moon program's end point
    moon_code.push(format!("% buffer space used for console output"));
    // buffer used by the lib.m subroutines
    moon_code.push(format!("buf       res 20"));
}

fn var_decl(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {}
fn assign_stat(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    // FIXME: Next 3 lines assume very simple VarElementList.
    let lhs_node_index = ast.get_child(node_index, 0);
    let lhs_memory_entry_index = get_memory_from_symbol(ast, lhs_node_index).unwrap();
    let lhs_variable_name = get_name(ast, lhs_node_index);
    let rhs_node_index = ast.get_child(node_index, 1);
    let rhs_memory_entry_index = ast.get_element(rhs_node_index).memory_table_entry.unwrap();
    // FIXME: This is only for debugging.
    let rhs_variable_name = get_name(ast, rhs_node_index);

    // Then, do the processing of this nodes' visitor
    // allocate local registers
    let register1 = ast.register_pool.pop();
    //generate code
    moon_code.push(format!(
        "% processing: {} := {}",
        lhs_variable_name, rhs_variable_name,
    ));
    // load the assigned value into a register
    moon_code.push(format!(
        "lw r{},{}(r14)",
        register1,
        get_offset(ast, rhs_node_index),
    ));
    // assign the value to the assigned variable
    moon_code.push(format!(
        "sw {}(r14),r{}",
        get_offset(ast, lhs_node_index),
        register1
    ));
    // deallocate local registers
    ast.register_pool.push(register1);
}

fn var_element_list(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    //FIXME: This should not be there. It could be in the table creation if get_var did not get variables.
    //FIXME: We assume there is only one element and that it is a variable.
    // let symbol_entry_index = ast.get_element(node_index).symbol_table_entry.unwrap();
    // let symbol_entry = ast.symbol_table_arena.get_table_entry(symbol_entry_index);
    // let symbol_type = match &symbol_entry.kind {
    //     SymbolKind::Variable(symbol_type) => symbol_type,
    //     _ => unreachable!(),
    // };
    let memory_entry_index = get_memory_from_symbol(ast, node_index).unwrap();
    // dbg!(&symbol_entry.name);
    // let memory_entry_index =
    //     find_variable_memory_entry(ast, node_index, &symbol_entry.name).unwrap();

    // let memory_entry_index = ast.memory_table_arena.new_memory_table_entry(
    //     VariableKind::Var(symbol_entry.name.clone()),
    //     symbol_to_variabl_type(&symbol_type),
    //     get_size(ast, symbol_type).unwrap(),
    // );
    ast.get_mut_element(node_index).memory_table_entry = Some(memory_entry_index);
}

fn binary_op(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    use NodeType::*;
    use OperatorType::*;

    let (rhs_node_index1, op_node_index, rhs_node_index2) =
        match ast.get_element(node_index).node_type {
            RelExpr => (
                ast.get_child(node_index, 0),
                ast.get_child(node_index, 1),
                ast.get_child(node_index, 2),
            ),
            AddOp | MultOp => (
                ast.get_child(node_index, 0),
                node_index,
                ast.get_child(node_index, 1),
            ),
            _ => unreachable!(),
        };

    // Get operation
    let operator = match ast
        .get_element(op_node_index)
        .token
        .as_ref()
        .unwrap()
        .token_type
    {
        TokenType::Operator(operator) => operator,
        _ => unreachable!(),
    };

    let operator_lexeme = ast
        .get_element(op_node_index)
        .token
        .as_ref()
        .unwrap()
        .lexeme
        .clone()
        .unwrap();

    // FIXME: Need to allow other operation than +
    // FIXME: Next 3 lines assume very simple VarElementList.
    let lhs_node_index = node_index;
    // let lhs_memory_entry_index = get_memory_entry(ast, lhs_node_index);
    let lhs_variable_name = get_name(ast, lhs_node_index);
    // let rhs_memory_entry_index1 = ast.get_element(rhs_node_index1).memory_table_entry.unwrap();
    let rhs_variable_name1 = get_name(ast, rhs_node_index1);
    // let rhs_memory_entry_index2 = ast.get_element(rhs_node_index2).memory_table_entry.unwrap();
    let rhs_variable_name2 = get_name(ast, rhs_node_index2);

    // Then, do the processing of this nodes' visitor
    // allocate registers to this subcomputation
    let register1 = ast.register_pool.pop();
    let register2 = ast.register_pool.pop();
    let register3 = ast.register_pool.pop();
    // generate code

    moon_code.push(format!(
        "% processing: {} := {} {} {}",
        lhs_variable_name, rhs_variable_name1, operator_lexeme, rhs_variable_name2,
    ));

    // load the values of the operands into registers
    moon_code.push(format!(
        "lw r{},{}(r14)",
        register1,
        get_offset(ast, rhs_node_index1),
    ));
    moon_code.push(format!(
        "lw r{},{}(r14)",
        register2,
        get_offset(ast, rhs_node_index2),
    ));

    match operator {
        LT => {
            // < operands
            moon_code.push(format!("clt r{},r{},r{}", register3, register1, register2,));
        }
        LEq => {
            // <= operands
            moon_code.push(format!("cle r{},r{},r{}", register3, register1, register2,));
        }
        NEq => {
            // != operands
            moon_code.push(format!("cne r{},r{},r{}", register3, register1, register2,));
        }
        GT => {
            // > operands
            moon_code.push(format!("cgt r{},r{},r{}", register3, register1, register2,));
        }
        GEq => {
            // >= operands
            moon_code.push(format!("cge r{},r{},r{}", register3, register1, register2,));
        }
        Eq => {
            // == operands
            moon_code.push(format!("ceq r{},r{},r{}", register3, register1, register2,));
        }
        Addition => {
            // add operands
            moon_code.push(format!("add r{},r{},r{}", register3, register1, register2,));
        }
        Subtraction => {
            // substract operands
            moon_code.push(format!("sub r{},r{},r{}", register3, register1, register2,));
        }
        Multiplication => {
            // multiply operands
            moon_code.push(format!("mul r{},r{},r{}", register3, register1, register2,));
        }
        Division => {
            // divide operands
            moon_code.push(format!("div r{},r{},r{}", register3, register1, register2,));
        }
        And => {
            // && operands
            let and_index = ast.register_pool.get_and();
            let zero = format!("zeroand{}", and_index);
            let end = format!("endand{}", and_index);
            moon_code.push(format!("bz r{},{}", register1, zero,));
            moon_code.push(format!("bz r{},{}", register2, zero,));
            moon_code.push(format!("addi r{},r0,1", register3));
            moon_code.push(format!("j {}", end));
            moon_code.push(format!("{:<10}addi r{},r0,0", zero, register3));
            moon_code.push(format!("{:<10}nop", end));
        }
        Or => {
            // || operands
            let or_index = ast.register_pool.get_and();
            let zero = format!("zeroor{}", or_index);
            let end = format!("endor{}", or_index);
            moon_code.push(format!("bz r{},{}", register1, zero,));
            moon_code.push(format!("bz r{},{}", register2, zero,));
            moon_code.push(format!("addi r{},r0,1", register3));
            moon_code.push(format!("j {}", end));
            moon_code.push(format!("{:<10}addi r{},r0,0", zero, register3));
            moon_code.push(format!("{:<10}nop", end));
        }
        _ => unreachable!(),
    }

    // assign the result into a temporary variable (assumed to have been previously created by the symbol table generator)
    moon_code.push(format!(
        "sw {}(r14),r{}",
        get_offset(ast, lhs_node_index),
        register3
    ));
    // deallocate the registers
    ast.register_pool.push(register1);
    ast.register_pool.push(register2);
    ast.register_pool.push(register3);
}

fn unary_op(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    //use NodeType::*;
    use OperatorType::*;

    let (lhs_node_index, operator, operator_lexeme, rhs_node_index) =
        match ast.get_element(node_index).node_type {
            NodeType::AssignStat | NodeType::AssignStati => (
                ast.get_child(node_index, 0),
                Assignment,
                "".to_string(),
                ast.get_child(node_index, 1),
            ),
            NodeType::Not => (
                node_index,
                Not,
                ast.get_element(node_index)
                    .token
                    .as_ref()
                    .unwrap()
                    .lexeme
                    .clone()
                    .unwrap(),
                ast.get_child(node_index, 0),
            ),
            _ => unreachable!(),
        };

    // FIXME: Need to allow other operation than +
    // FIXME: Next 3 lines assume very simple VarElementList.
    //let lhs_node_index = node_index;
    // let lhs_memory_entry_index = get_memory_entry(ast, lhs_node_index);
    let lhs_variable_name = get_name(ast, lhs_node_index);
    // let rhs_memory_entry_index1 = ast.get_element(rhs_node_index1).memory_table_entry.unwrap();
    let rhs_variable_name = get_name(ast, rhs_node_index);

    // Then, do the processing of this nodes' visitor
    // allocate registers to this subcomputation
    let register1 = ast.register_pool.pop();
    let register2 = ast.register_pool.pop();
    let register3 = ast.register_pool.pop();
    // generate code

    moon_code.push(format!(
        "% processing: {} := {} {}",
        lhs_variable_name, operator_lexeme, rhs_variable_name,
    ));

    // load the values of the operands into registers
    moon_code.push(format!(
        "lw r{},{}(r14)",
        register1,
        get_offset(ast, rhs_node_index),
    ));

    match operator {
        Assignment => {}
        Not => {
            // ! operands
            let not_index = ast.register_pool.get_not();
            let zero = format!("zeroor{}", not_index);
            let end = format!("end0r{}", not_index);
            moon_code.push(format!("not r{},r{}", register2, register1));
            moon_code.push(format!("bz r{},{}", register2, zero,));
            moon_code.push(format!("addi r{},r0,1", register3));
            moon_code.push(format!("j {}", end));
            moon_code.push(format!("{:<10}addi r{},r0,0", zero, register3));
            moon_code.push(format!("{:<10}nop", end));
        }
        _ => unreachable!(),
    }

    // assign the result into a temporary variable (assumed to have been previously created by the symbol table generator)
    moon_code.push(format!(
        "sw {}(r14),r{}",
        get_offset(ast, lhs_node_index),
        register3
    ));
    // deallocate the registers
    ast.register_pool.push(register1);
    ast.register_pool.push(register2);
    ast.register_pool.push(register3);
}

fn num(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    let data = ast
        .get_element(node_index)
        .token
        .as_ref()
        .unwrap()
        .lexeme
        .as_ref()
        .unwrap()
        .clone();
    // Then, do the processing of this nodes' visitor
    // create a local variable and allocate a register to this subcomputation
    let register1 = ast.register_pool.pop();
    // generate code
    moon_code.push(format!(
        "% processing: {} := {}",
        get_name(ast, node_index),
        data
    ));
    // create a value corresponding to the literal value
    moon_code.push(format!("addi r{},r0,{}", register1, data));
    // assign this value to a temporary variable (assumed to have been previously created by the symbol table generator)
    moon_code.push(format!(
        "sw {}(r14),r{}",
        get_offset(ast, node_index),
        register1
    ));
    // deallocate the register for the current node
    ast.register_pool.push(register1);
}

fn write(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    let child_node_index = ast.get_child(node_index, 0);
    let child_memory_entry_index = get_memory_from_symbol(ast, child_node_index).unwrap();
    let child_variable_name = get_name(ast, child_node_index);
    let function_stack_frame_size = get_current_function_offset(ast, node_index);
    // Then, do the processing of this nodes' visitor
    // create a local variable and allocate a register to this subcomputation
    let register1 = ast.register_pool.pop();
    //generate code
    moon_code.push(format!("% processing: write({})", child_variable_name,));
    // put the value to be printed into a register
    moon_code.push(format!(
        "lw r{},{}(r14)",
        register1,
        get_offset(ast, child_node_index),
    ));
    moon_code.push(format!("% put value on stack"));
    // make the stack frame pointer point to the called function's stack frame
    // FIXME: Not sure about the size to add here.
    moon_code.push(format!("addi r14,r14,{}", function_stack_frame_size));
    // copy the value to be printed in the called function's stack frame
    moon_code.push(format!("sw -8(r14),r{}", register1));
    moon_code.push(format!("% put buffer address on the stack"));
    moon_code.push(format!("addi r{},r0,buf", register1));
    moon_code.push(format!("sw -12(r14),r{}", register1));
    moon_code.push(format!("% convert int to string for output"));
    moon_code.push(format!("jl r15,intstr"));
    // receive the return value in r13
    moon_code.push(format!("sw -8(r14),r13"));
    moon_code.push(format!("% output to console"));
    // putstr is expecting the address of the string in r1
    //moon_code.push(format!("lw r1,-8(r14)"));
    moon_code.push(format!("jl r15,putstr"));
    // make the stack frame pointer point back to the current function's stack frame
    moon_code.push(format!("subi r14,r14,{}", function_stack_frame_size));
    // deallocate local register
    ast.register_pool.push(register1);
}

fn get_name(ast: &AST, node_index: usize) -> String {
    let entry_index = ast.get_element(node_index).memory_table_entry.unwrap();
    let memory_entry = ast.memory_table_arena.get_table_entry(entry_index);
    memory_entry.get_name()
}

fn get_offset(ast: &AST, node_index: usize) -> isize {
    let entry_index = ast.get_element(node_index).memory_table_entry.unwrap();
    let memory_entry = ast.memory_table_arena.get_table_entry(entry_index);
    memory_entry.get_offset()
}

fn get_memory_from_symbol(ast: &AST, node_index: usize) -> Option<usize> {
    use NodeType::*;

    let symbol_entry_index = ast.get_element(node_index).symbol_table_entry.unwrap();
    let symbol_entry = ast.symbol_table_arena.get_table_entry(symbol_entry_index);
    let symbol_name = &symbol_entry.name;

    let (function_node_index, _) = ast
        .get_parent_node_of_type(node_index, &[MainFuncBody, FuncDef])
        .unwrap();
    find_variable_memory_entry(ast, function_node_index, symbol_name)
}

fn find_variable_memory_entry(
    ast: &AST,
    node_index: usize,
    variable_name: &String,
) -> Option<usize> {
    let memory_table_index = ast.get_element(node_index).memory_table.unwrap();
    //let memory_table = ast.memory_table_arena.get_table_entry(memory_table_index);
    for &entry_index in ast.memory_table_arena.get_table_entries(memory_table_index) {
        let memory_entry = ast.memory_table_arena.get_table_entry(entry_index);
        // FIXME: I should probably only check for real variable.
        if *variable_name == memory_entry.get_name() {
            return Some(entry_index);
        }
    }
    None
}

fn get_current_function_offset(ast: &AST, node_index: usize) -> isize {
    use NodeType::*;

    let (function_node_index, _) = ast
        .get_parent_node_of_type(node_index, &[MainFuncBody, FuncDef])
        .unwrap();
    let memory_table_index = ast.get_element(function_node_index).memory_table.unwrap();
    let memory_table = ast.memory_table_arena.get_table(memory_table_index);
    memory_table.offset
}

fn get_memory_entry(ast: &AST, node_index: usize) -> usize {
    ast.get_element(node_index).memory_table_entry.unwrap()
}
