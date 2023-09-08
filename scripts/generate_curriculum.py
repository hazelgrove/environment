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

        count = 0
        for e in exp[:i]:
            if e[1] == '!':
                count += 1
        body_output.append((join_exp(temp_exp, op), len(op) + i + count)) # Just change that variable to hole
        
        if i != 0:
            temp_exp = temp_exp[i:]
            temp_op = op[i:]
            body_output.append((join_exp(temp_exp, temp_op), len(temp_op))) # Remove everything before that variable
        
        temp_exp = exp.copy()
        if temp_exp[i][1] == '!':
            temp_exp[i] = '(!(?))'
            body_output.append((join_exp(temp_exp, op), len(op) + i + count + 1)) # Just change that variable to hole
            
    
    output = [(header + '\n' + b + '\n' + assertion, i) for b, i in body_output]
    
    return output
    

def generate_curriculum(assignment: str):
    assignment = assignment.strip().split('\n')
    if len(assignment) != 4:
        raise ValueError('Bad assignment formatting')

    return permutate(assignment[0].strip(), assignment[1].strip(), (assignment[2] + '\n' + assignment[3]).strip())
    

def find_max_num(files):
    max_num = -1
    for f in files:
        f = f[:-3]
        if f.isnumeric():
            max_num = int(f) if int(f) > max_num else max_num
    
    return max_num + 1

    
if __name__ == '__main__':
    base_dir = 'data/curriculum_gen_tests/'
    template_dir = 'templates/two_var'
    out_dir = 'two_var_gen'
    
    for assignment_file in os.scandir(os.path.join(base_dir,template_dir)):
        if not re.match(r'.*\.ml',assignment_file.name):
            pass
        with open(assignment_file.path,'r') as file :
            assignment = file.read()
        assignments = generate_curriculum(assignment)
        for a, i in assignments:
            targ_dir = os.path.join(base_dir,out_dir,f'{i}')
            if not os.path.exists(targ_dir):
                os.makedirs(targ_dir)
                with open(os.path.join(targ_dir,'test.ml'), 'w') as f:
                    f.write("[]\n")
                
            dir_list = os.listdir(targ_dir)
            
            with open(os.path.join(targ_dir,f'{find_max_num(dir_list)}.ml'), 'w') as f:
                f.write(a)
    
    
    print(generate_curriculum(assignment))