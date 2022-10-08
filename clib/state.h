#ifndef __STATE_H
#define __STATE_H

#define MAX_NUM_NODES 100
#define MAX_NUM_TESTS 10
#define NUM_ACTIONS 63
#define MAX_TREE_LENGTH 10000
#define MAX_NUM_VARS 11

/*
Struct defining the AST structure
*/
typedef struct
{
    /*
    Edge showing indices of vertices on each edge and which child it belongs to.

    Edges are in the format : (begin, end, descriptor)
    For non-negative descriptors, the number representes that the edge connects to the n-th child of the parent.
    For descriptor == -1, the edge shows the binding of a variable.
    */
    int edges[MAX_NUM_NODES * 3][3];

    /*
    Node descriptor of each node.

    See ocamllib/astparser/ast.ml for the int to descriptor mapping
    */
    int tests[MAX_NUM_TESTS][2];

    /*
    Node descriptor of each node.

    See ocamllib/astparser/ast.ml for the int to descriptor mapping
    */
    int nodes[MAX_NUM_NODES];

    int starter[MAX_NUM_NODES];

    /*
    Actions permitted in the current AST.

    For the n-th action, permistted_actions[n] is 1 if it is permitted, and 0 otherwise.
    */
    int permitted_actions[NUM_ACTIONS + MAX_NUM_VARS * 2];

    int vars_in_scope[MAX_NUM_VARS];

    int args_in_scope[MAX_NUM_VARS][2];

    char zast[MAX_TREE_LENGTH];

    /*
    The index of the root of the tree.
    */
    int cursor;

    /*
    The number of nodes
    */
    int num_nodes;

    /*
    The number of edges
    */
    int num_edges;

    /*
    The number of tests
    */
    int num_tests;

    /*
    The number of variables in scope
    */
    int num_vars;

    /*
    The number of arguments in scope
    */
    int num_args;

    /*
    The index of the assignment
    */
    int assignment;

    /*
    The index of the code
    */
    int code;
} State;

#endif
