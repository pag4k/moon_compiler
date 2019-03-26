use crate::ast_node::*;
use crate::ast_visitor::*;
use crate::code_generation_common::*;
use crate::code_generation_error::*;
use crate::memory_table::*;
use crate::symbol_table::*;

use std::collections::HashMap;

pub fn code_generator_visitor(ast: &mut AST) -> Vec<String> {
    use NodeType::*;
    let mut moon_code: Vec<String> = Vec::new();
    let mut semantic_actions: SemanticActionMap<String> = HashMap::new();
    semantic_actions.insert(Prog, prog);
    semantic_actions.insert(MainFuncBody, main_func_body);
    semantic_actions.insert(AssignStat, assign_stat);
    semantic_actions.insert(AssignStati, assign_stat);
    semantic_actions.insert(VarDecl, var_decl);
    semantic_actions.insert(VarElementList, var_element_list);

    // semantic_actions.insert(RelExpr, temp_var);
    // semantic_actions.insert(AddOp, temp_var);
    // semantic_actions.insert(MultOp, temp_var);
    // semantic_actions.insert(Not, temp_var);
    // semantic_actions.insert(Sign, temp_var);
    semantic_actions.insert(Num, num);
    semantic_actions.insert(Write, write);

    ast_traversal(ast, &mut moon_code, &semantic_actions);
    moon_code
}

fn prog(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {}

fn main_func_body(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    // generate moon program's entry point
    moon_code.insert(0, format!("entry\n"));
    // make the stack frame pointer (address stored in r14) point
    // to the top address allocated to the moon processor
    moon_code.insert(1, format!("addi r14,r0,topaddr\n"));

    // generate moon program's end point
    moon_code.push(format!("% buffer space used for console output\n"));
    // buffer used by the lib.m subroutines
    moon_code.push(format!("buf       res 20\n"));
    // halting point of the entire program
    moon_code.push(format!("hlt\n"));
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
        "% processing: {} := {}\n",
        lhs_variable_name, rhs_variable_name,
    ));
    // load the assigned value into a register
    moon_code.push(format!(
        "lw {},{}(r14)\n",
        register1,
        get_offset(ast, rhs_node_index),
    ));
    // assign the value to the assigned variable
    moon_code.push(format!(
        "sw {}(r14),{}\n",
        get_offset(ast, lhs_node_index),
        register1
    ));
    // deallocate local registers
    ast.register_pool.push(register1);
}

fn var_element_list(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    //FIXME: This should not be there. It could be in the table creation if get_var did not get variables.
    //FIXME: We assume there is only one element and that it is a variable.
    let symbol_entry_index = ast.get_element(node_index).symbol_table_entry.unwrap();
    let symbol_entry = ast.symbol_table_arena.get_table_entry(symbol_entry_index);
    let symbol_type = match &symbol_entry.kind {
        SymbolKind::Variable(symbol_type) => symbol_type,
        _ => unreachable!(),
    };

    let memory_entry_index = ast.memory_table_arena.new_memory_table_entry(
        VariableKind::Var(symbol_entry.name.clone()),
        symbol_to_variabl_type(&symbol_type),
        get_size(ast, symbol_type).unwrap(),
    );
    ast.get_mut_element(node_index).memory_table_entry = Some(memory_entry_index);
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
        "% processing: {} := {}\n",
        get_name(ast, node_index),
        data
    ));
    // create a value corresponding to the literal value
    moon_code.push(format!("addi r{},r0,{}\n", register1, data));
    // assign this value to a temporary variable (assumed to have been previously created by the symbol table generator)
    moon_code.push(format!(
        "sw {}(r14),r{}\n",
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
    // Then, do the processing of this nodes' visitor
    // create a local variable and allocate a register to this subcomputation
    let register1 = ast.register_pool.pop();
    let register2 = ast.register_pool.pop();
    //generate code
    moon_code.push(format!("% processing: write({})\n", child_variable_name,));
    // put the value to be printed into a register
    moon_code.push(format!(
        "lw {},{}(r14)\n",
        register1,
        get_offset(ast, child_node_index),
    ));
    moon_code.push(format!("% put value on stack\n"));
    // make the stack frame pointer point to the called function's stack frame
    // m_moonExecCode += m_mooncodeindent + "addi r14,r14," + p_node.m_symtab.m_size + "\n";
    // // copy the value to be printed in the called function's stack frame
    // m_moonExecCode += m_mooncodeindent + "sw -8(r14)," + localregister1 + "\n";
    // m_moonExecCode += m_mooncodeindent + "% link buffer to stack\n";
    // m_moonExecCode += m_mooncodeindent + "addi " + localregister1 + ",r0, buf\n";
    // m_moonExecCode += m_mooncodeindent + "sw -12(r14)," + localregister1 + "\n";
    // m_moonExecCode += m_mooncodeindent + "% convert int to string for output\n";
    // m_moonExecCode += m_mooncodeindent + "jl r15, intstr\n";
    // // receive the return value in r13 and right away put it in the next called function's stack frame
    // m_moonExecCode += m_mooncodeindent + "sw -8(r14),r13\n";
    // m_moonExecCode += m_mooncodeindent + "% output to console\n";
    // m_moonExecCode += m_mooncodeindent + "jl r15, putstr\n";
    // // make the stack frame pointer point back to the current function's stack frame
    // m_moonExecCode += m_mooncodeindent + "subi r14,r14," + p_node.m_symtab.m_size + "\n";
    // //deallocate local register
    // this.m_registerPool.push(localregister1);
    // this.m_registerPool.push(localregister2);
}

fn get_name(ast: &AST, node_index: usize) -> String {
    let entry_index = ast.get_element(node_index).memory_table_entry.unwrap();
    let memory_entry = ast.memory_table_arena.get_table_entry(entry_index);
    memory_entry.get_name()
}

fn get_offset(ast: &AST, node_index: usize) -> usize {
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
