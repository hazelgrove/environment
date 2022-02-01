#include <stdio.h>
#include <stdint.h>

#define MAX_NUM_NODES 10
#define NUM_ACTIONS 10

/*
Struct defining the AST structure

Example
    # graph (for node descriptors 'a' and 'b'):
    0.    1.    2. 
    a --> b --> a

    # observation:
    {
    "nodes": [
        0, # a
        1, # b
        0, # a
    ],
    "edges": [
        [0, 1],
        [1, 2],
    ],
    "permitted_actions": [...]
    }
*/
typedef struct{
    int nodes[MAX_NUM_NODES]; // Node descriptor of each node
    int edges[MAX_NUM_NODES * MAX_NUM_NODES][2]; // Edge showing indices of vertices on each edge
    int permitted_actions[NUM_ACTIONS]; // Actions permitted in the current AST
} State;

/*
Mutates AST according to action taken

Input:
    - ast: struct representing the AST
    - action: index of action

Mutates:
    - ast->nodes, ast->edges
*/
void take_action(State *ast, int action){

}

/*
Test the code given by the AST on the unit test

Input:
    - ast: struct representing the AST
    - unit_test_index: index of unit test

Output:
    - 0 for not passed; 1 for passed
*/
int check_ast(const State *ast, int unit_test_index){

    return 0;
}

/*
Find the valid actions given the AST

Input:
    - ast: struct representing the AST

Mutates:
    - ast->permitted_actions
*/
void valid_actions(State *ast){

}

/*
Get the original ast of the code

Input:
    - ast: struct representing the AST
    - index: index of code submission

Mutates:
    - ast
*/
void get_ast(State *ast, int index){

}

