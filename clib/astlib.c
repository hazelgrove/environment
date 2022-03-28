#include "astlib.h"


/*
Mutates AST according to action taken

Input:
    - ast: struct representing the AST
    - action: index of action

Mutates:
    - ast->nodes, ast->edges
*/
void take_action(State *ast, int action){
    change_ast(action);
    copy_ast(ast, &curr_state);
}


/*
Test the code given by the AST on the unit test

Input:
    - ast: struct representing the AST
    - unit_test_index: index of unit test

Output:
    - 0 for not passed; 1 for passed
*/
int check_ast(State *ast){
    copy_ast(&curr_state, ast);
    return run_unit_tests();
}


/*
Find the valid actions given the AST

Input:
    - ast: struct representing the AST

Mutates:
    - ast->permitted_actions
*/
void valid_actions(State *ast){
    for (int i = 0; i < NUM_ACTIONS; i++)
        ast->permitted_actions[i] = 1;
    
    curr_state = *ast;
}


/*
Get the original ast of the code

Input:
    - ast: struct representing the AST
    - index: index of code submission

Mutates:
    - ast
*/
void init_assignment(State *ast, int assignment, int index){
    load_starter_code(assignment, index);
    load_tests(assignment);

    curr_state.assignment = assignment;
    curr_state.code = index;
    
    copy_ast(ast, &curr_state);
}


/*
Print the current state as a line of code

Input:
    - ast: struct representing the AST
*/
void print_curr_state(State *ast){
    copy_ast(&curr_state, ast);
    print_code();
}


/*
Initiate the OCaml code and set default values of curr_state
*/
void init_c(){
    // Build a stub argv[] to satisfy caml_Startup()
    char *argv[2];
    argv[0] = "";
    argv[1] = NULL;
    caml_startup(argv);

    curr_state.num_nodes = 0;
    for (int i = 0; i < MAX_NUM_NODES; i++){
        curr_state.nodes[i] = -1;
    }

    curr_state.num_edges = 0;
    for (int i = 0; i < MAX_NUM_NODES * MAX_NUM_NODES; i++){
        curr_state.edges[i][0] = -1;
        curr_state.edges[i][1] = -1;
        curr_state.edges[i][2] = -1;
    }

    for (int i = 0; i < NUM_ACTIONS; i++){
        curr_state.permitted_actions[i] = 0;
    }

    curr_state.num_tests = 0;
    for (int i = 0; i < MAX_NUM_TESTS; i++){
        curr_state.tests[i][0] = -1;
        curr_state.tests[i][1] = -1;
    }

    curr_state.assignment = -1;
    curr_state.code = -1;
}


/*
Shut down the OCaml Code
*/
void close_c(){
    caml_shutdown();
}


/*
Copy the AST from astsrc to astdst

Input:
    - astdst: destination to copy to
    - astsrc: source to copy from

Mutates:
    - astdst
*/
void copy_ast(State *astdst, const State *astsrc){
    for (int i = 0; i < MAX_NUM_NODES; i++){
        astdst->nodes[i] = astsrc->nodes[i];
    }
    for (int i = 0; i < MAX_NUM_NODES * MAX_NUM_NODES; i++){
        astdst->edges[i][0] = astsrc->edges[i][0];
        astdst->edges[i][1] = astsrc->edges[i][1];
        astdst->edges[i][2] = astsrc->edges[i][2];
    }
    for (int i = 0; i < NUM_ACTIONS; i++){
        astdst->permitted_actions[i] = astsrc->permitted_actions[i];
    }
    for (int i = 0; i < MAX_NUM_TESTS; i++){
        astdst->tests[i][0] = astsrc->tests[i][0];
        astdst->tests[i][1] = astsrc->tests[i][1];
    }
    astdst->root = astsrc->root;
    astdst->num_nodes = astsrc->num_nodes;
    astdst->num_edges = astsrc->num_edges;
    astdst->num_tests = astsrc->num_tests;
    astdst->assignment = astsrc->assignment;
    astdst->code = astsrc->code;
}

