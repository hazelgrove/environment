#! /usr/bin/env python3
import argparse
from os import path
import json
import os
from itertools import product, chain,combinations
import random
import re
import shutil
import yaml
from curriculum_gen_helper import Node, make_curriculum
from collections import defaultdict
from tqdm import tqdm
from copy import copy
import sympy as S
import numpy as np

## ------------- Function generation --------------
# generate boolean functions of n variables according to
# parameters. 
# simplify and save those functions. 

def powerset(in_list:list):
    "powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"
    return chain.from_iterable(combinations(in_list, r) for r in range(len(in_list)+1))

def make_nfuncs(n,simplify=True,variations=False): 

    print(simplify,variations)
    print('Genrating functions...')
    in_vars = S.symbols([f'x{n}' for n in range(1,n+1)]) # x1, x2, ... , xn 
    inputs = list(product([0,1],repeat=n))
    funcs = [S.SOPform(in_vars,incs) for incs in tqdm(powerset(inputs),total=2**len(inputs))]
    print(f'{len(funcs)} total functions generated')
    print('simplifying...')
    if simplify: 
        funcs = list(tqdm(map(S.simplify_logic,funcs), total=len(funcs)))
    elif variations: 
        print('variations are being gnerated')
        # print(f'generating reorderings: initial size = {len(funcs)}')
        funcs_new = []
        for func in tqdm(funcs):
            cnf, dnf = S.simplify_logic(func,form='cnf'), S.simplify_logic(func, form='dnf')
            if cnf != dnf: 
                funcs_new.extend([cnf,dnf])
            else: 
                funcs_new.append(cnf)
        funcs = funcs_new 
        print(f'generating reorderings: final size: {len(funcs)}')
    return funcs, in_vars

def sympy_to_ocaml(expr): 
    if type(expr) is S.Not: 
        return f'(!({sympy_to_ocaml(expr.args[0])}))'
    elif type(expr) is S.And or type(expr) is S.Or: 
        op =  ' && ' if type(expr) is S.And else ' || '
        ret_string = sympy_to_ocaml(expr.args[0])
        for child in expr.args[1:]: 
            ret_string = f'( {op.join([ret_string,sympy_to_ocaml(child)])} )'
        return ret_string
        # n-ary ops 
    elif type(expr) is S.Symbol: 
        return str(expr)
    elif  expr is S.true: 
        return 'true'
    elif expr is S.false: 
        return 'false ' 
    else: 
        raise NotImplementedError(f'unknown expr {expr}')


def make_assert(func,in_vars):
    ttable = S.logic.boolalg.truth_table(func,in_vars)
    true_false_map = {0: "false", 1: "true "}
    clauses = []
    for x_values, is_true in ttable:
        clause = " ".join(true_false_map[val] for val in x_values)
        clause = f"(f {clause})"
        if not is_true:  # case is false: negate it
            clause = f"(!{clause})"
        clauses.append(clause)
    return "assert (" + " && ".join(clauses) + ")\n"


def make_test_strings(funcs,in_vars,assert_funcs=None):
    # allow our user to pass in 'template' functions in the case that 
    # the original function was disassembled
    if  assert_funcs is None: 
        assert_funcs = funcs 
    asserts = map(lambda x: make_assert(x,in_vars), assert_funcs)
    if type(funcs[0]) is Node: 
        ocaml_funcs = map(lambda x: x.to_ocaml(), funcs)
    else: 
        ocaml_funcs = map(sympy_to_ocaml,funcs)
    # convert from ttvar to string in the format that we want...
    pretty_vars = [str(var) for var in in_vars]
    header = "let f " + " ".join(f"({var} : bool)" for var in pretty_vars) + " ="
    # make the actual strings
    strings = []
    for func, assert_ in zip(ocaml_funcs, asserts):
        strings.append(f"{header}\n\t{func}\nin\n{assert_}\n")
    return strings


## --- curriculum generation section ---
#
# Generate a series of one-action removed intermediate steps for a given function
# then save those files in a format amennable to the environment/agent system
#


def write_test_dir(tests, num, targ_dir):
    # make fresh folder. remove all other 'conflicting' files
    dirname = os.path.join(targ_dir, str(num))
    if os.path.isdir(dirname):
        for file in os.scandir(dirname):
            if re.match("(\d+\.ml)", file.name):
                os.remove(file.path)
    else:
        os.makedirs(dirname)
    # make vestigial test file
    with open(os.path.join(dirname, "test.ml"), "w") as file:
        file.write("[]\n")
    # actually write our files
    for i, test in enumerate(tests):
        with open(os.path.join(dirname, f"{i}.ml"), "w") as file:
            file.write(test)


def gen_curricula(funcs, vars,mns_correction = 1.5,gen_variations=False,verbose=True):
    print('Generating curricula...')
    curriculum = defaultdict(lambda: [])
    max_num_steps = 1
    max_num_nodes = 0
    for func in tqdm(funcs):
        # get our curriculum for that file, unzip into two lists
        solution_template = Node.from_sympy(func)
        test_funcs, cursor_starts,max_steps = make_curriculum(solution_template,gen_variations=gen_variations,verbose=verbose)
        # print(test_funcs, cursor_starts)
        max_num_steps = max(max_num_steps, max_steps + 1)
        max_num_nodes = max(max_num_nodes, solution_template.size())
        # generate full test functions
        test_strings = make_test_strings(test_funcs,vars,assert_funcs=[func]*len(cursor_starts))
        for test_str, cursor_pos in zip(test_strings, cursor_starts):
            curriculum[cursor_pos].append(test_str)
    total_tests = sum(len(tests) for _, tests in curriculum.items())
    print('Done.')
    # calculate number of nodes outside of function body....
    out_of_body =  (2**len(vars))*(len(vars)*2  +4) + len(vars) 
    #func def = one per variable... 
    print(f'largest solution had {max_num_nodes} nodes in func body; {max_num_nodes+ out_of_body} total')
    print(f'{total_tests} total tests in curriculum')
    max_num_steps = int(max_num_steps * mns_correction)
    return curriculum, max_num_steps

## ------------------ IO section  ------------------
#
# Handle IO for user input, and formatting and saving our tests
#

def parse_args():
    parser = argparse.ArgumentParser(
        description="generate sboolean functions of n variables."
    )
    parser.add_argument("n_args", help="number of args to generate", type=int)
    parser.add_argument("targ_dir", help="dir to write templates to", type=str)
    parser.add_argument("-r", "--raw",help="if set, we don't simplify our funcs", action="store_true")  # on/off flag
    parser.add_argument(
        "-c",
        "--curriculum",
        help="generate curiculum to target directory rather than just templates",
        action="store_true",
    )  # on/off flag
    parser.add_argument("-t", "--test_split", help="If specified, a test split will be selected at random comprising this portion of the total functions",type=float, default=None)
    parser.add_argument("--seed", help="Seed for random state.", default=42)
    parser.add_argument("--mns_correction", help="correction factor for max_num steps. 1.5 is good base. if things are converging to lower numbers, bump this up.",default=1.5,type=float)
    parser.add_argument("-v",'--verbose',action="store_true")
    parser.add_argument('--select',type=str,default=None,help="select only a subset of template functions to use. Primarily used for debugging. Specified as json-formatted list of ints.")
    parser.add_argument('--variations', help="generate variations of high-level functions to attempt to augment data",type=lambda x: x.lower() == 'true')  # on/off flag
    parser.add_argument('--permutations', help="generate low level permutations of functions, to further augment data",type=lambda x: x.lower() == 'true')  # on/off flag
    return parser.parse_args()


def save_raw_tests(num_vars,templates,targ_dir): 
    # clear any existing template files 
    for elt in os.scandir(targ_dir): 
        if re.match(r'\d+',elt.name) and elt.is_dir(): 
            shutil.rmtree(elt.path,)
    # make new directory
    train_dir = os.path.join(targ_dir,'0')
    os.mkdir(train_dir)
    # write files
    with open(os.path.join(train_dir,'test.ml'),'w') as file: 
        file.write('[]\n')
    for i, temp_str in enumerate(templates):
        with open(path.join(train_dir, f"{i}.ml"), "w") as file:
            # preprocess: mask answer 
            test_str = temp_str.split('=')[0] + "=\n\t?\nin" + temp_str.split('in')[-1]
            file.write(test_str)
    return make_raw_param_snippet(num_vars,len(templates),targ_dir)


def save_template_strings(template_strings, template_dir):
    if not path.isdir(template_dir):
        os.makedirs(template_dir)
    # clear out any old elements 
    for file in os.scandir(template_dir): 
        if file.is_file() and re.match(r'\d+\.ml',file.name): 
            os.remove(file.path)
    # write templates 
    for i, template_string in enumerate(template_strings):
        with open(path.join(template_dir, f"{i}.ml"), "w") as file:
            file.write(template_string)


def make_raw_param_snippet(num_vars, num_tests,targ_dir): 
    num_steps_per_size = {
        3:70,
        2:18,
        1:5, 
    }
    params = {
            "assignment_dir": os.path.join("data", targ_dir.split("data/")[-1]),
            "num_assignments": 1,
            "code_per_assignment": [num_tests],
            "cursor_start_pos": [0],
            "max_episode_steps": num_steps_per_size[num_vars],
            "done_action": True,
            "perturbation": 0,
        }
    return params


def save_curriculum(
    curriculum, targ_dir, max_num_steps, done_action=True
):
    # print these in order small to big:
    max_start_pos = max(curriculum.keys())
    cursor_starts, assignment_nums = [], []
    for i in range(max_start_pos + 1):
        if len(curriculum[i]) > 0:
            # write dir
            write_test_dir(curriculum[i], len(cursor_starts), targ_dir)
            cursor_starts.append(i)
            assignment_nums.append(len(curriculum[i]))

    # save config snippet
    params = {
            "assignment_dir": os.path.join("data", targ_dir.split("data/")[-1]),
            "num_assignments": len(cursor_starts),
            "code_per_assignment": assignment_nums,
            "cursor_start_pos": cursor_starts,
            "max_episode_steps": max_num_steps,
            "done_action": done_action,
            "perturbation": 0,
        }
    return params




## ---------------- Driver functions ---------------------
# Handle high level logic for coordinating function and curriculum gen 
# and saving and writing logic
# 

def split_folds(comps,targ_dir, split, seed=42):
    # rezip this so that each element is paired (ttable, pretty_func, func)
    comps = copy(comps)
    random.shuffle(comps)
    num_test = int(len(comps) * split) if split < 1 else int(split)
    test, train = comps[:num_test], comps[num_test:]
    return {'env':(path.join(targ_dir,'train'),train),'eval':(path.join(targ_dir,'test'),test)}

def seed_all(seed=42): 
    random.seed(seed)
    np.random.seed(seed)

def main(args): 
    if args.variations: print('creating variations on functions')
    if args.permutations: print('generating permutations')
    seed_all(args.seed)
    funcs, varnames = make_nfuncs(args.n_args,simplify=not (args.raw or args.variations),variations=args.variations)
    targ_dir = args.targ_dir if not args.curriculum else path.join(args.targ_dir,'curriculum')
    if args.test_split and not args.select: 
        folds = split_folds(funcs,targ_dir,args.test_split,args.seed)
    elif args.select is not None: 
        if args.verbose: 
            for i, t in enumerate(funcs): 
                print(i,t)
        print('pairing down args')
        select_tests = json.loads(args.select)
        selected = [test for num, test in enumerate(funcs) if num in select_tests]
        assert(max(select_tests) < len(funcs))
        folds = {'env':(targ_dir,selected)}
        print(f'pared down to {len(selected)} tests')
    
    else: 
        folds = {'env':(targ_dir,funcs)}
    arg_strings ={}
    for name, (targ_dir, funcs) in folds.items(): 
        test_strings = make_test_strings(funcs,varnames)
        save_template_strings(test_strings, path.join(targ_dir,'templates'))
        if args.curriculum:

            curriculum, max_steps = gen_curricula(funcs,varnames,mns_correction=args.mns_correction,gen_variations=args.permutations,verbose=args.verbose)
            arg_strings[name] = save_curriculum(curriculum, targ_dir, max_steps)
        else: 
            arg_strings[name] = save_raw_tests(args.n_args,test_strings,targ_dir)
    with open(path.join(args.targ_dir,'param_snippet.yaml'),'w') as pfile:
        yaml.dump(arg_strings,pfile)
    print(f'saved params file to {targ_dir}')

if __name__ == "__main__":
    args = parse_args()
    main(args)