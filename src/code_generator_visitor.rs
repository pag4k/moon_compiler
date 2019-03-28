use crate::ast_node::*;
use crate::ast_visitor::*;
use crate::code_generation_common::*;
use crate::code_generation_error::*;
use crate::language::*;
use crate::memory_table::*;
use crate::symbol_table::*;

use std::collections::HashMap;

const INDENT: &str = "          ";
const ENTRY_MARKER: &str = "% ENTRY MARKER";
const FUNCTION_MARKER: &str = "% FUNCTION MARKER";
const IF_MARKER: &str = "% IF MARKER";
const ELSE_MARKER: &str = "% ELSE MARKER";
const FOR_COND_MARKER: &str = "% FOR COND MARKER";
const FOR_INCR_MARKER: &str = "% FOR INCR MARKER";
const FOR_LOOP_MARKER: &str = "% FOR LOOP MARKER";

pub fn code_generator_visitor(ast: &mut AST) -> Vec<String> {
    use NodeType::*;
    let mut moon_code: Vec<String> = Vec::new();
    let mut semantic_actions: SemanticActionMap<String> = HashMap::new();
    semantic_actions.insert(Prog, prog);
    semantic_actions.insert(MainFuncBody, main_func_body);
    semantic_actions.insert(FuncDef, func_def);
    semantic_actions.insert(FunctionCall, function_call);

    // Statements
    semantic_actions.insert(VarDecl, var_decl);
    semantic_actions.insert(IfStat, if_stat);
    semantic_actions.insert(ForStat, for_stat);
    semantic_actions.insert(WriteStat, write);

    // Binary operation
    semantic_actions.insert(RelExpr, binary_op);
    semantic_actions.insert(AddOp, binary_op);
    semantic_actions.insert(MultOp, binary_op);
    // Unary operation
    semantic_actions.insert(AssignStat, unary_op);
    semantic_actions.insert(AssignForStat, unary_op);
    semantic_actions.insert(Not, unary_op);
    semantic_actions.insert(Sign, unary_op);

    // Variables
    semantic_actions.insert(VarElementList, var_element_list);
    semantic_actions.insert(Num, num);

    // Marker
    semantic_actions.insert(Type, leaf);
    semantic_actions.insert(Id, leaf);
    semantic_actions.insert(Idi, leaf);
    semantic_actions.insert(StatBlock, leaf);

    ast_traversal(ast, &mut moon_code, &semantic_actions);
    moon_code
}

fn prog(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {}

fn main_func_body(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    add_entry_marker(ast, moon_code, node_index);

    let mut entry_moon_code: Vec<String> = Vec::new();

    // generate moon program's entry point
    entry_moon_code.push(format!("{}entry", INDENT));
    // make the stack frame pointer (address stored in r14) point
    // to the top address allocated to the moon processor
    entry_moon_code.push(format!("{}addi r14,r0,topaddr", INDENT));

    push_at(moon_code, entry_moon_code, ENTRY_MARKER);

    // halting point of the entire program
    moon_code.push(format!("{}hlt", INDENT));

    // DATA SECTION
    // generate moon program's end point
    moon_code.push(format!("{}% buffer space used for console output", INDENT));
    // buffer used by the lib.m subroutines
    moon_code.push(format!("buf       res 20"));
}

fn func_def(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    let func_variable_name = ast
        .get_element(ast.get_child(node_index, 2))
        .token
        .as_ref()
        .unwrap()
        .lexeme
        .as_ref()
        .unwrap();

    let mut function_moon_code: Vec<String> = Vec::new();
    function_moon_code.push(format!(
        "{}% processing function definition: {}",
        INDENT, func_variable_name
    ));
    //create the tag to jump onto
    // and copy the jumping-back address value in the called function's stack frame
    function_moon_code.push(format!("{:<10}sw -4(r14),r15", func_variable_name));
    push_at(moon_code, function_moon_code, FUNCTION_MARKER);

    // copy back the jumping-back address into r15
    moon_code.push(format!("{}lw r15,-4(r14)", INDENT));
    // jump back to the calling function
    moon_code.push(format!("{}jr r15", INDENT));
    moon_code.push("".to_string());
}

fn function_call(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {}

		// pass parameters
		// here we assume that the parameters are the size of a word, 
		// which is not true for arrays and objects. 
		// In those cases, a loop copying the values e.g. byte-by-byte is necessary
		// SymTabEntry tableentryofcalledfunction = p_node.m_symtab.lookupName(p_node.getData());
		// int indexofparam = 0;
		// m_moonExecCode += m_mooncodeindent + "% processing: function call to "  + p_node.getChildren().get(0).m_moonVarName + " \n";
		// for(Node param : p_node.getChildren().get(1).getChildren()){
		// 	m_moonExecCode += m_mooncodeindent + "lw " + localregister1 + "," + p_node.m_symtab.lookupName(param.m_moonVarName).m_offset + "(r14)\n";
		// 	int offsetofparam = p_node.m_symtab.m_size + tableentryofcalledfunction.m_subtable.m_symlist.get(indexofparam).m_offset;
		// 	m_moonExecCode += m_mooncodeindent + "sw " + offsetofparam + "(r14)," + localregister1 + "\n";
		// 	indexofparam++;
		// }
		// // make the stack frame pointer point to the called function's stack frame
		// m_moonExecCode += m_mooncodeindent + "addi r14,r14," + p_node.m_symtab.m_size + "\n";
		// // jump to the called function's code
		// // here the function's name is the label
		// // a unique label generator is necessary in the general case
		// m_moonExecCode += m_mooncodeindent + "jl r15," + p_node.getData() + "\n";
		// // upon jumping back, set the stack frame pointer back to the current function's stack frame  
		// m_moonExecCode += m_mooncodeindent + "subi r14,r14," + p_node.m_symtab.m_size + "\n";
		// // copy the return value in memory space to store it on the current stack frame
		// // to evaluate the expression in which it is 
		// m_moonExecCode += m_mooncodeindent + "lw " + localregister1 + "," + p_node.m_symtab.m_size + "(r14)\n";
		// m_moonExecCode += m_mooncodeindent + "sw " + p_node.m_symtab.lookupName(p_node.m_moonVarName).m_offset + "(r14)," + localregister1 + "\n";
		// this.m_registerPool.push(localregister1);	
}

fn var_decl(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {}

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
        "{}% processing: {} := {} {} {}",
        INDENT, lhs_variable_name, rhs_variable_name1, operator_lexeme, rhs_variable_name2,
    ));

    // load the values of the operands into registers
    moon_code.push(format!(
        "{}lw r{},{}(r14)",
        INDENT,
        register1,
        get_offset(ast, rhs_node_index1),
    ));
    moon_code.push(format!(
        "{}lw r{},{}(r14)",
        INDENT,
        register2,
        get_offset(ast, rhs_node_index2),
    ));

    match operator {
        LT => {
            // < operands
            moon_code.push(format!(
                "{}clt r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        LEq => {
            // <= operands
            moon_code.push(format!(
                "{}cle r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        NEq => {
            // != operands
            moon_code.push(format!(
                "{}cne r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        GT => {
            // > operands
            moon_code.push(format!(
                "{}cgt r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        GEq => {
            // >= operands
            moon_code.push(format!(
                "{}cge r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        Eq => {
            // == operands
            moon_code.push(format!(
                "{}ceq r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        Addition => {
            // add operands
            moon_code.push(format!(
                "{}add r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        Subtraction => {
            // substract operands
            moon_code.push(format!(
                "{}sub r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        Multiplication => {
            // multiply operands
            moon_code.push(format!(
                "{}mul r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        Division => {
            // divide operands
            moon_code.push(format!(
                "{}div r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        And => {
            // && operands
            let and_index = ast.register_pool.get_and();
            let zero = format!("zeroand{}", and_index);
            let end = format!("endand{}", and_index);
            moon_code.push(format!("{}bz r{},{}", INDENT, register1, zero,));
            moon_code.push(format!("{}bz r{},{}", INDENT, register2, zero,));
            moon_code.push(format!("{}addi r{},r0,1", INDENT, register3));
            moon_code.push(format!("{}j {}", INDENT, end));
            moon_code.push(format!("{:<10}addi r{},r0,0", zero, register3));
            moon_code.push(format!("{:<10}nop", end));
        }
        Or => {
            // || operands
            let or_index = ast.register_pool.get_and();
            let zero = format!("zeroor{}", or_index);
            let end = format!("endor{}", or_index);
            moon_code.push(format!("{}bz r{},{}", INDENT, register1, zero,));
            moon_code.push(format!("{}bz r{},{}", INDENT, register2, zero,));
            moon_code.push(format!("{}addi r{},r0,1", INDENT, register3));
            moon_code.push(format!("{}j {}", INDENT, end));
            moon_code.push(format!("{:<10}addi r{},r0,0", zero, register3));
            moon_code.push(format!("{:<10}nop", end));
        }
        _ => unreachable!(),
    }

    // assign the result into a temporary variable (assumed to have been previously created by the symbol table generator)
    moon_code.push(format!(
        "{}sw {}(r14),r{}",
        INDENT,
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
            NodeType::AssignStat | NodeType::AssignForStat => (
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
            NodeType::Sign => (
                node_index,
                Subtraction,
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
        "{}% processing: {} := {} {}",
        INDENT, lhs_variable_name, operator_lexeme, rhs_variable_name,
    ));

    // load the values of the operands into registers
    moon_code.push(format!(
        "{}lw r{},{}(r14)",
        INDENT,
        register1,
        get_offset(ast, rhs_node_index),
    ));

    match operator {
        Assignment => {}
        Subtraction => {
            moon_code.push(format!("{}sub r{},r0,r{}", INDENT, register3, register1,));
        }
        Not => {
            // ! operands
            let not_index = ast.register_pool.get_not();
            let zero = format!("zeroor{}", not_index);
            let end = format!("end0r{}", not_index);
            moon_code.push(format!("{}not r{},r{}", INDENT, register2, register1));
            moon_code.push(format!("{}bz r{},{}", INDENT, register2, zero,));
            moon_code.push(format!("{}addi r{},r0,1", INDENT, register3));
            moon_code.push(format!("{}j {}", INDENT, end));
            moon_code.push(format!("{:<10}addi r{},r0,0", zero, register3));
            moon_code.push(format!("{:<10}nop", end));
        }
        _ => unreachable!(),
    }

    // assign the result into a temporary variable (assumed to have been previously created by the symbol table generator)
    moon_code.push(format!(
        "{}sw {}(r14),r{}",
        INDENT,
        get_offset(ast, lhs_node_index),
        register3
    ));
    // deallocate the registers
    ast.register_pool.push(register1);
    ast.register_pool.push(register2);
    ast.register_pool.push(register3);
}

fn num(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    add_entry_marker(ast, moon_code, node_index);
    add_if_marker(ast, moon_code, node_index);
    add_for_marker(ast, moon_code, node_index);

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
        "{}% processing: {} := {}",
        INDENT,
        get_name(ast, node_index),
        data
    ));
    // create a value corresponding to the literal value
    moon_code.push(format!("{}addi r{},r0,{}", INDENT, register1, data));
    // assign this value to a temporary variable (assumed to have been previously created by the symbol table generator)
    moon_code.push(format!(
        "{}sw {}(r14),r{}",
        INDENT,
        get_offset(ast, node_index),
        register1
    ));
    // deallocate the register for the current node
    ast.register_pool.push(register1);
}

fn if_stat(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    let cond_node_index = ast.get_child(node_index, 0);
    let cond_memory_entry_index = get_memory_entry(ast, cond_node_index);
    let cond_variable_name = get_name(ast, cond_node_index);

    let register1 = ast.register_pool.pop();

    let if_index = ast.register_pool.get_not();
    let elseif = format!("elseif{}", if_index);
    let endif = format!("endif{}", if_index);

    // ADD THIS BEFORE IF BLOCK
    let mut if_moon_code: Vec<String> = Vec::new();
    if_moon_code.push(format!(
        "{}% processing: if({}), index: {}",
        INDENT, cond_variable_name, if_index
    ));
    if_moon_code.push(format!(
        "{}lw r{},{}(r14)",
        INDENT,
        register1,
        get_offset(ast, cond_node_index),
    ));
    if_moon_code.push(format!("{}bz r{},{}", INDENT, register1, elseif));
    ast.register_pool.push(register1);
    /////

    push_at(moon_code, if_moon_code, IF_MARKER);

    // ADD THIS BEFORE ELSE BLOCK
    let mut else_moon_code: Vec<String> = Vec::new();
    else_moon_code.push(format!("{}j {}", INDENT, endif));
    else_moon_code.push(format!("{:<10}nop", elseif));
    /////

    push_at(moon_code, else_moon_code, ELSE_MARKER);

    moon_code.push(format!("{:<10}nop", endif));
}

fn for_stat(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    let assign_node_index = node_index;
    let assign_memory_entry_index = get_memory_entry(ast, assign_node_index);
    let assign_variable_name = get_name(ast, assign_node_index);
    let cond_node_index = ast.get_child(node_index, 3);;

    let register1 = ast.register_pool.pop();
    //let register2 = ast.register_pool.pop();
    //let register3 = ast.register_pool.pop();

    let for_index = ast.register_pool.get_for();
    let condfor = format!("condfor{}", for_index);
    let incrfor = format!("incrfor{}", for_index);
    let loopfor = format!("loopfor{}", for_index);
    let endfor = format!("endfor{}", for_index);

    // ADD THIS BEFORE FOR CONDITION
    let mut cond_moon_code: Vec<String> = Vec::new();

    cond_moon_code.push(format!(
        "{}% processing: for_cond({}), index: {}",
        INDENT, assign_variable_name, for_index
    ));

    // Assignment
    let rhs_node_index = ast.get_child(node_index, 2);
    let rhs_variable_name = get_name(ast, rhs_node_index);

    cond_moon_code.push(format!(
        "{}% processing: {} := {}",
        INDENT, assign_variable_name, rhs_variable_name,
    ));
    cond_moon_code.push(format!(
        "{}lw r{},{}(r14)",
        INDENT,
        register1,
        get_offset(ast, rhs_node_index),
    ));
    cond_moon_code.push(format!(
        "{}sw {}(r14),r{}",
        INDENT,
        get_offset(ast, assign_node_index),
        register1
    ));
    // Conditional label
    cond_moon_code.push(format!("{:<10}nop", condfor));

    ast.register_pool.push(register1);
    //ast.register_pool.push(register2);

    push_at(moon_code, cond_moon_code, FOR_COND_MARKER);

    // ADD THIS BEFORE FOR INCREMENT
    let register1 = ast.register_pool.pop();

    let mut incr_moon_code: Vec<String> = Vec::new();

    incr_moon_code.push(format!(
        "{}% processing: for_incr({}), index: {}",
        INDENT, assign_variable_name, for_index
    ));
    incr_moon_code.push(format!(
        "{}lw r{},{}(r14)",
        INDENT,
        register1,
        get_offset(ast, cond_node_index),
    ));
    incr_moon_code.push(format!("{}bz r{},{}", INDENT, register1, endfor));
    incr_moon_code.push(format!("{}j {}", INDENT, loopfor));
    incr_moon_code.push(format!("{:<10}nop", incrfor));
    ast.register_pool.push(register1);

    push_at(moon_code, incr_moon_code, FOR_INCR_MARKER);

    // ADD THIS BEFORE FOR STATBLOCK
    let mut loop_moon_code: Vec<String> = Vec::new();
    loop_moon_code.push(format!("{}j {}", INDENT, condfor));
    loop_moon_code.push(format!("{:<10}nop", loopfor));

    push_at(moon_code, loop_moon_code, FOR_LOOP_MARKER);

    // ADD THIS AFTER THE LOOP.
    moon_code.push(format!("{}j {}", INDENT, incrfor));
    moon_code.push(format!("{:<10}nop", endfor));
}

fn write(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    let child_node_index = ast.get_child(node_index, 0);
    let child_memory_entry_index = get_memory_entry(ast, child_node_index);
    let child_variable_name = get_name(ast, child_node_index);
    let function_stack_frame_size = get_current_function_offset(ast, node_index);
    // Then, do the processing of this nodes' visitor
    // create a local variable and allocate a register to this subcomputation
    let register1 = ast.register_pool.pop();
    //generate code
    moon_code.push(format!(
        "{}% processing: write({})",
        INDENT, child_variable_name,
    ));
    // put the value to be printed into a register
    moon_code.push(format!(
        "{}lw r{},{}(r14)",
        INDENT,
        register1,
        get_offset(ast, child_node_index),
    ));
    moon_code.push(format!("{}% put value on stack", INDENT));
    // make the stack frame pointer point to the called function's stack frame
    // FIXME: Not sure about the size to add here.
    moon_code.push(format!(
        "{}addi r14,r14,{}",
        INDENT, function_stack_frame_size
    ));
    // copy the value to be printed in the called function's stack frame
    moon_code.push(format!("{}sw -8(r14),r{}", INDENT, register1));
    moon_code.push(format!("{}% put buffer address on the stack", INDENT));
    moon_code.push(format!("{}addi r{},r0,buf", INDENT, register1));
    moon_code.push(format!("{}sw -12(r14),r{}", INDENT, register1));
    moon_code.push(format!("{}% convert int to string for output", INDENT));
    moon_code.push(format!("{}jl r15,intstr", INDENT));
    // receive the return value in r13
    moon_code.push(format!("{}sw -8(r14),r13", INDENT));
    moon_code.push(format!("{}% output to console", INDENT));
    // putstr is expecting the address of the string in r1
    //moon_code.push(format!("lw r1,-8(r14)"));
    moon_code.push(format!("{}jl r15,putstr", INDENT));
    // make the stack frame pointer point back to the current function's stack frame
    moon_code.push(format!(
        "{}subi r14,r14,{}",
        INDENT, function_stack_frame_size
    ));
    // deallocate local register
    ast.register_pool.push(register1);
}

fn leaf(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    add_entry_marker(ast, moon_code, node_index);
    add_function_marker(ast, moon_code, node_index);
    add_if_marker(ast, moon_code, node_index);
    add_for_marker(ast, moon_code, node_index);
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
        .get_parent_node_of_type(node_index, &[MainFuncBody, FuncDef], &[])
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
        .get_parent_node_of_type(node_index, &[MainFuncBody, FuncDef], &[])
        .unwrap();
    let memory_table_index = ast.get_element(function_node_index).memory_table.unwrap();
    let memory_table = ast.memory_table_arena.get_table(memory_table_index);
    memory_table.offset
}

fn get_memory_entry(ast: &AST, node_index: usize) -> usize {
    ast.get_element(node_index).memory_table_entry.unwrap()
}

fn add_entry_marker(ast: &AST, moon_code: &mut Vec<String>, node_index: usize) {
    use NodeType::*;

    // If the node has children, return.
    if !ast.get_children(node_index).is_empty() {
        return;
    }

    // If the node has a MainFuncBody parent.
    if let Some((main_func_body_index, _)) =
        ast.get_parent_node_of_type_on_left_branch(node_index, &[MainFuncBody], &[])
    {
        moon_code.push(ENTRY_MARKER.to_string());
    }
}

fn add_function_marker(ast: &AST, moon_code: &mut Vec<String>, node_index: usize) {
    use NodeType::*;

    // If the node has children, return.
    if !ast.get_children(node_index).is_empty() {
        return;
    }

    // If the node has a MainFuncBody parent.
    if let Some((func_def_index, _)) =
        ast.get_parent_node_of_type_on_left_branch(node_index, &[FuncDef], &[])
    {
        moon_code.push(FUNCTION_MARKER.to_string());
    }
}

fn add_if_marker(ast: &AST, moon_code: &mut Vec<String>, node_index: usize) {
    use NodeType::*;

    // If the node has children, return.
    if !ast.get_children(node_index).is_empty() {
        return;
    }

    let stat_block_node_index = if let StatBlock = ast.get_element(node_index).node_type {
        node_index
    } else {
        // If the node does not have StatBlock parent, return.
        match ast.get_parent_node_of_type_on_left_branch(node_index, &[StatBlock], &[]) {
            Some((stat_block_node_index, _)) => stat_block_node_index,
            None => {
                return;
            }
        }
    };
    //println!("{}", stat_block_node_index);
    let grand_parent_node_index = ast.get_parent(stat_block_node_index).unwrap();
    // If it is a IfStat, add one of the two marker.
    if let IfStat = ast.get_element(grand_parent_node_index).node_type {
        // If it has a right sibling, it is the if block, if it has not, it is the else block.
        if ast.get_right_sibling(stat_block_node_index).is_some() {
            moon_code.push(IF_MARKER.to_string());
        } else {
            moon_code.push(ELSE_MARKER.to_string());
        }
    }
}

fn add_for_marker(ast: &AST, moon_code: &mut Vec<String>, node_index: usize) {
    use NodeType::*;

    // If the node has children, return.
    if !ast.get_children(node_index).is_empty() {
        return;
    }

    // If the node has a RelExpr parent.
    if let Some((rel_expr_node_index, _)) =
        ast.get_parent_node_of_type_on_left_branch(node_index, &[RelExpr], &[])
    {
        // If parent is a ForStat, add FOR_COND_MARKER.
        let grand_parent_node_index = ast.get_parent(rel_expr_node_index).unwrap();
        if let ForStat = ast.get_element(grand_parent_node_index).node_type {
            moon_code.push(FOR_COND_MARKER.to_string());
        }
    }

    // If the node has a AssignForStat parent.
    if let Some((assign_for_star_node_index, _)) =
        ast.get_parent_node_of_type_on_left_branch(node_index, &[AssignForStat], &[])
    {
        // If parent is a ForStat, add FOR_INCR_MARKER.
        let grand_parent_node_index = ast.get_parent(assign_for_star_node_index).unwrap();
        if let ForStat = ast.get_element(grand_parent_node_index).node_type {
            moon_code.push(FOR_INCR_MARKER.to_string());
        }
    }

    let stat_block_node_index = if let StatBlock = ast.get_element(node_index).node_type {
        Some(node_index)
    } else {
        // If the node has a StatBlock parent.
        match ast.get_parent_node_of_type_on_left_branch(node_index, &[StatBlock], &[]) {
            Some((stat_block_node_index, _)) => Some(stat_block_node_index),
            None => None,
        }
    };

    if let Some(stat_block_node_index) = stat_block_node_index {
        let grand_parent_node_index = ast.get_parent(stat_block_node_index).unwrap();
        // If it is a ForStat, add FOR_LOOP_MARKER.
        if let ForStat = ast.get_element(grand_parent_node_index).node_type {
            println!("{}", node_index);
            moon_code.push(FOR_LOOP_MARKER.to_string());
        }
    }
}

fn push_at(moon_code: &mut Vec<String>, new_moon_code: Vec<String>, marker: &str) {
    let index = get_marker_index(moon_code, marker);
    moon_code.splice(index..=index, new_moon_code.into_iter());
}

fn get_marker_index(moon_code: &mut Vec<String>, marker: &str) -> usize {
    moon_code.len()
        - 1
        - moon_code
            .iter()
            .rev()
            .position(|line| *line == marker)
            .unwrap()
}
