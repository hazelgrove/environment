#include "astlib.h"

/*
Mutates AST according to action taken

Input:
    - ast: struct representing the AST
    - action: index of action

Mutates:
    - ast->nodes, ast->edges
*/
void take_action(State *ast, int action)
{
    change_zast(action);
    get_ast();
    get_cursor_info();
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
int check_ast(State *ast)
{
    copy_ast(&curr_state, ast);
    return run_unit_tests();
}

/*
Get the original ast of the code

Input:
    - ast: struct representing the AST
    - code: index of code submission

Mutates:
    - ast
*/
void init_assignment(State *ast, char *dir, int assignment, int code, int n, int cursor)
{
    load_starter_code(dir, assignment, code, n, cursor);
    get_ast();
    load_tests(dir, assignment);

    curr_state.assignment = assignment;
    curr_state.code = code;

    get_cursor_info();

    copy_ast(ast, &curr_state);
}

/*
Print the current state as a line of code

Input:
    - ast: struct representing the AST
*/
void print_curr_state(State *ast)
{
    copy_ast(&curr_state, ast);
    printf("Zippered AST in Sexp form: %s\n", curr_state.zast);
    printf("Cursor Position: %d\n", curr_state.cursor);
    printf("Variables in scope: ");
    for (int i = 0; i < MAX_NUM_VARS; i++)
    {
        if (curr_state.vars_in_scope[i] != -1)
            printf("%d ", curr_state.vars_in_scope[i]);
    }
    printf("\n");
    printf("Permitted Actions: ");
    for (int i = 0; i < NUM_ACTIONS + MAX_NUM_VARS * 2; i++)
    {
        if (curr_state.permitted_actions[i] == 1)
            printf("%d ", i);
    }
    printf("\n");
    print_code();
    fflush(stdout);
}

/*
Initiate the OCaml code and set default values of curr_state
*/
void init_c(int seed)
{
    // Build a stub argv[] to satisfy caml_Startup()
    char *argv[2];
    argv[0] = "";
    argv[1] = NULL;
    caml_startup(argv);

    init(seed);

    curr_state.num_nodes = 0;
    for (int i = 0; i < MAX_NUM_NODES; i++)
    {
        curr_state.nodes[i] = -1;
        curr_state.starter[i] = -1;
    }

    curr_state.num_edges = 0;
    for (int i = 0; i < MAX_NUM_NODES * 3; i++)
    {
        curr_state.edges[i][0] = -1;
        curr_state.edges[i][1] = -1;
        curr_state.edges[i][2] = -1;
    }

    for (int i = 0; i < NUM_ACTIONS + MAX_NUM_VARS * 2; i++)
    {
        curr_state.permitted_actions[i] = 0;
    }

    curr_state.num_tests = 0;
    for (int i = 0; i < MAX_NUM_TESTS; i++)
    {
        curr_state.tests[i][0] = -1;
        curr_state.tests[i][1] = -1;
    }

    curr_state.num_vars = 0;
    for (int i = 0; i < MAX_NUM_VARS; i++)
    {
        curr_state.vars_in_scope[i] = -1;
    }

    curr_state.num_args = 0;
    for (int i = 0; i < MAX_NUM_VARS; i++)
    {
        curr_state.args_in_scope[i][0] = -1;
        curr_state.args_in_scope[i][1] = -1;
    }

    curr_state.assignment = -1;
    curr_state.code = -1;

    strcpy(curr_state.zast, "");
}

/*
Shut down the OCaml Code
*/
void close_c()
{
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
void copy_ast(State *astdst, const State *astsrc)
{
    for (int i = 0; i < MAX_NUM_NODES; i++)
    {
        astdst->nodes[i] = astsrc->nodes[i];
        astdst->starter[i] = astsrc->starter[i];
    }
    for (int i = 0; i < MAX_NUM_NODES * 3; i++)
    {
        astdst->edges[i][0] = astsrc->edges[i][0];
        astdst->edges[i][1] = astsrc->edges[i][1];
        astdst->edges[i][2] = astsrc->edges[i][2];
    }
    for (int i = 0; i < NUM_ACTIONS + MAX_NUM_VARS * 2; i++)
    {
        astdst->permitted_actions[i] = astsrc->permitted_actions[i];
    }
    for (int i = 0; i < MAX_NUM_TESTS; i++)
    {
        astdst->tests[i][0] = astsrc->tests[i][0];
        astdst->tests[i][1] = astsrc->tests[i][1];
    }
    for (int i = 0; i < MAX_NUM_VARS; i++)
    {
        astdst->vars_in_scope[i] = astsrc->vars_in_scope[i];
    }
    for (int i = 0; i < MAX_NUM_VARS; i++)
    {
        astdst->args_in_scope[i][0] = astsrc->args_in_scope[i][0];
        astdst->args_in_scope[i][1] = astsrc->args_in_scope[i][1];
    }
    astdst->cursor = astsrc->cursor;
    astdst->num_nodes = astsrc->num_nodes;
    astdst->num_edges = astsrc->num_edges;
    astdst->num_tests = astsrc->num_tests;
    astdst->num_vars = astsrc->num_vars;
    astdst->num_args = astsrc->num_args;
    astdst->assignment = astsrc->assignment;
    astdst->code = astsrc->code;
}
