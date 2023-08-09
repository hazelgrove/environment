import re
import os
from typing import List, Tuple, Union

def join_exp(exp: List[str], op: List[str]) -> str:
    if len(exp) != len(op) + 1:
        raise ValueError('Incorrect length of expressions and operators')

    output = exp[0]
    for i in range(len(op)):
        output = output + ' ' + op[i] + ' ' + exp[i + 1]
        
    return output


# Outputs a list of tuples (curriculum, index of hole)
def permutate(
    header: str,
    body: str,
    assertion: str,
    operators: List[str] = ['||', '&&']
) -> List[Tuple[str, int]]:
    body_output = []
    
    body = body.split(' ')
    exp = [e for e in body if e not in operators]
    op = [e for e in body if e in operators]
    
    for i in range(len(exp)):
        temp_exp = exp.copy()
        temp_exp[i] = '?'

        body_output.append((join_exp(temp_exp, op), len(op) + i)) # Just change that variable to hole
        
        if i != 0:
            temp_exp = temp_exp[i:]
            temp_op = op[i:]
            body_output.append((join_exp(temp_exp, temp_op), len(temp_op))) # Remove everything before that variable
    
    output = [(header + '\n' + b + '\n' + assertion, i) for b, i in body_output]
    
    return output
    

def generate_curriculum(assignment: str):
    assignment = assignment.strip().split('\n')
    if len(assignment) != 4:
        raise ValueError('Bad assignment formatting')

    return permutate(assignment[0].strip(), assignment[1].strip(), (assignment[2] + '\n' + assignment[3]).strip())
    

    
if __name__ == '__main__':
    assignment = '''
        let f ( x1 : bool ) ( x2 : bool )= 
            x1 || x2
        in 
        assert ((!(f false false)) && (f false true) && (f true false) && (f true true))
    '''
    print(generate_curriculum(assignment))