#ifndef __STATE_H
#define __STATE_H

#define MAX_NUM_NODES 5
#define NUM_ACTIONS 5

/*
Struct defining the AST structure

Example:
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
    int edges[MAX_NUM_NODES * MAX_NUM_NODES][2]; // Edge showing indices of vertices on each edge (TODO: should be preorder traversal?)
    int permitted_actions[NUM_ACTIONS]; // Actions permitted in the current AST
} State;

#endif