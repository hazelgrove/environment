#! /usr/bin/env python3
import argparse
from os import path
import os
from itertools import product, chain,combinations
import random
import re
import yaml
from curriculum_gen_helper import Node, make_curriculum
from collections import defaultdict
from tqdm import tqdm
from sklearn.model_selection import train_test_split
import sympy as S


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
        "--generate curiculum to target directory rather than just templates",
        action="store_true",
    )  # on/off flag
    parser.add_argument("-t", "--test_split",type=float, default=None)
    parser.add_argument("--seed", default=42)
    return parser.parse_args()



def powerset(in_list:list):
    "powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"
    return chain.from_iterable(combinations(in_list, r) for r in range(len(in_list)+1))

def make_nfuncs(n,simplify=True): 
    in_vars = S.symbols([f'x{n}' for n in range(1,n+1)]) # x1, x2, ... , xn 
    inputs = list(product([0,1],repeat=n))
    funcs = [S.SOPform(in_vars,incs) for incs in powerset(inputs)]
    print(f'{len(funcs)} total functions generated')
    print('simplifying...')
    if simplify: 
        funcs = list(map(S.simplify,funcs))
    print('Done.')
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

def save_test_strings(test_strings, template_dir):
    if not path.isdir(template_dir):
        os.makedirs(template_dir)
    for i, test_string in enumerate(test_strings):
        with open(path.join(template_dir, f"{i}.ml"), "w") as file:
            file.write(test_string)


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


def gen_curricula(funcs, vars):
    print('Generating curricula...')
    curriculum = defaultdict(lambda: [])
    max_num_steps = 1
    for func in tqdm(funcs):
        # get our curriculum for that file, unzip into two lists
        test_funcs, cursor_starts = make_curriculum(Node.from_sympy(func))
        # print(test_funcs, cursor_starts)
        max_num_steps = max(max_num_steps, len(test_funcs) + 1)
        # generate full test functions
        test_strings = make_test_strings(test_funcs,vars,assert_funcs=[func]*len(cursor_starts))
        for test_str, cursor_pos in zip(test_strings, cursor_starts):
            curriculum[cursor_pos].append(test_str)
    total_tests = sum(len(tests) for _, tests in curriculum.items())
    print('Done.')
    print(f'{total_tests} total tests in curriculum')
    return curriculum, max_num_steps


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
    json_str = {
        "env": {
            "assignment_dir": os.path.join("/RL_env/data", targ_dir.split("data/")[-1]),
            "num_assignments": len(cursor_starts),
            "code_per_assignment": assignment_nums,
            "cursor_start_pos": cursor_starts,
            "max_episode_steps": max_num_steps,
            "done_action": done_action,
            "perturbation": 0,
        }
    }
    with open(os.path.join(targ_dir, "param_snippet.yaml"), "w") as file:
        yaml.dump(json_str, file)

def split_folds(comps,targ_dir, split, seed=42):
    random.seed(seed)
    # rezip this so that each element is paired (ttable, pretty_func, func)
    comps_out = train_test_split(*comps,test_size=split,random_state=seed)
    train,test = comps_out[::2], comps_out[1::2]
    print(f'{len(train[0])} elements in train, {len(test[0])} elements in test')
    return {path.join(targ_dir,'train'):train,path.join(targ_dir,'test'):test}

def main(args): 
    funcs, varnames = make_nfuncs(args.n_args,simplify=not args.raw)
    if args.test_split: 
        folds = split_folds((funcs),args.targ_dir,args.test_split,args.seed)
    else: 
        folds = {args.targ_dir:funcs}

    for targ_dir, funcs in folds.items(): 
        if args.curriculum:
            curriculum, max_steps = gen_curricula(funcs,varnames)
            save_curriculum(curriculum, targ_dir, max_steps)
        else:
            test_strings = make_test_strings(funcs,varnames)
            save_test_strings(test_strings, args.targ_dir)


if __name__ == "__main__":
    args = parse_args()
    main(args)