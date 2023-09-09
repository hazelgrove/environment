#! /usr/bin/env python3
import re
import os
import shutil
import sys
import yaml
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
    
    body = re.split(r'\s+',body)
    exp = [e for e in body if e not in operators and e not in '()']
    op = [e for e in body if e in operators]
    for i in range(len(exp)):
        temp_exp = exp.copy()
        temp_exp[i] = '?'
        count = 0
        # what does this do? 
        for e in exp[:i]:
            if '!' in e:
                count += 1
        body_output.append((join_exp(temp_exp, op), len(op) + i + count)) # Just change that variable to hole
        
        if i != 0:
            temp_exp = temp_exp[i:]
            temp_op = op[i:]
            body_output.append((join_exp(temp_exp, temp_op), len(temp_op))) # Remove everything before that variable
        
        temp_exp = exp.copy()
        if '!' in temp_exp[i]:
            temp_exp[i] = temp_exp[i].replace('!','')
            body_output.append((join_exp(temp_exp, op), len(op) + i + count )) # Just change that variable to hole
            
    
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

def gen_for_template_dir(in_dir,out_dir): 
    max_num_steps = 1 
    for assignment_file in os.scandir(in_dir):
        if not re.match(r'.*\.ml',assignment_file.name):
            continue
        with open(assignment_file.path,'r') as file :
            assignment = file.read()
        assignments = generate_curriculum(assignment)
        for a, i in assignments:
            targ_dir = os.path.join(out_dir,f'{i}')
            if not os.path.exists(targ_dir):
                os.makedirs(targ_dir)
                with open(os.path.join(targ_dir,'test.ml'), 'w') as f:
                    f.write("[]\n")
            dir_list = os.listdir(targ_dir)
            with open(os.path.join(targ_dir,f'{find_max_num(dir_list)}.ml'), 'w') as f:
                f.write(a)
        max_num_steps = max(max_num_steps,len(assignments)+1)
    return max_num_steps

def make_config_portion(targ_dir,max_num_steps,done_action=True): 
    assignments = []
    for assn_dir in os.scandir(targ_dir): 
        if assn_dir.is_dir() and re.match(r'\d+',assn_dir.name):
            num_files = sum(1 for file in os.scandir(assn_dir) if re.match(r'\d+.ml',file.name) and file.is_file())
            assignments.append(num_files)
    #build json string 
    json_str = { 'env':{
        'assignment_dir':os.path.join('/RL_env/data',targ_dir.split('data/')[-1]),
        'num_assignments':len(assignments),
        'code_per_assignment': assignments,
        'cursor_start_pos': list(range(len(assignments))),
        'max_episode_steps': max_num_steps,
        'done_action':done_action,
        'perturbation':0
    }}
    with open(os.path.join(targ_dir,'params_snippet.yaml'),'w') as file: 
        yaml.dump(json_str,file)

    

if __name__ == '__main__':
    # handle args  
    base_dir = 'data/curriculum_gen_tests/'
    template_dir = 'templates/two_var'
    out_dir = 'two_var_gen'
    if len(sys.argv) ==4: 
        base_dir = sys.argv[1]
        template_dir = sys.argv[2]
        out_dir = sys.argv[3]

    in_dir = os.path.join(base_dir,template_dir)
    out_dir = os.path.join(base_dir,out_dir)
    
    if len(sys.argv) ==3: 
        in_dir = sys.argv[1]
        out_dir = sys.argv[2]

    if os.path.isdir(out_dir): 
        shutil.rmtree(out_dir)
    # run on folders
    steps = gen_for_template_dir(in_dir,out_dir)
    make_config_portion(out_dir,steps)
