#! /usr/bin/env python3
import argparse
from os import path
import os
from itertools import product
import random
from more_itertools import chunked, collapse
import re
import pyeda.inter as bools
from pyeda.boolalg.expr import AndOp, OrOp, NotOp, Variable, expr
from pyeda.boolalg.table import ttvar
import yaml
from curriculum_gen_helper import Node, make_curriculum
from collections import defaultdict
from copy import copy
from tqdm import tqdm
from sklearn.model_selection import train_test_split


def parse_args():
    parser = argparse.ArgumentParser(
        description="generate sboolean functions of n variables."
    )
    parser.add_argument("n_args", help="number of args to generate", type=int)
    parser.add_argument("targ_dir", help="dir to write templates to", type=str)
    parser.add_argument("-s", "--simplify", action="store_true")  # on/off flag
    parser.add_argument(
        "-c",
        "--curriculum",
        "--generate curiculum to target directory rather than just templates",
        action="store_true",
    )  # on/off flag
    parser.add_argument("-t", "--test_split",type=float, default=None)
    parser.add_argument("--seed", default=42)
    return parser.parse_args()


def make_nfuncs(n, simplify=False, verbose=False):
    ttables = []
    x = bools.ttvars("x", n)
    for tt_vals in product(*[range(2)] * (2**n)):
        ttables.append(bools.truthtable(x, tt_vals))
    print(len(tt_vals), len(ttables))
    # now use our fancy schmansy solver to solve everything
    # chunk into max-size of 500 tables at a time
    funcs = list(
        collapse(bools.espresso_tts(*chunk) for chunk in chunked(ttables, 500))
    )
    if simplify:
        funcs = list(map(lambda x: expr(x).simplify(), x))

    if verbose:
        ocaml_funcs = []
        for i, func in enumerate(funcs):
            if i < 10:
                print(f"\n RUnning func {i}")
            ocaml_funcs.append(prettyPrint(func, verbose=(i < 12)))
        print(ocaml_funcs[:3])
    else:
        ocaml_funcs = list(map(prettyPrint, funcs))
    return ttables, ocaml_funcs, funcs


def prettyPrint(expr, verbose=False):
    # define recursive function
    def pretty_print_helper(input, verbose=True):
        if input[0] in ["and", "or"]:
            if verbose:
                print(len(input), input)
            # binop
            binop = (
                "&&" if input[0] == "and" else "||" if input[0] == "or" else input[0]
            )
            ret_string = pretty_print_helper(input[1], verbose=verbose)
            if verbose:
                print(ret_string)
            for i in range(2, len(input)):
                right = pretty_print_helper(input[i], verbose=verbose)
                if verbose:
                    print(right)
                ret_string = f"( {ret_string} {binop} {right} )"
                if verbose:
                    print(ret_string)
            return ret_string
        elif len(input) == 2 and input[0] == "const":
            # atom: Boolean constant
            return "true" if input[1] else "false"
        elif len(input) == 2 and input[0] == "lit":
            # atom: other
            if input[1] < 0:  # negated
                return f"(!x{-input[1]})"
            else:
                return f" x{input[1]}"
        else:
            print(f"ERROR WITH INPUT {input}")
            raise ValueError("this shouldn't happen")

    # call on ast-ified version
    ast = expr.to_ast()
    return pretty_print_helper(ast, verbose=verbose)


def make_assert(truth_table):
    true_false_map = {0: "false", 1: "true "}
    clauses = []
    for rel in truth_table.iter_relation():
        clause = " ".join(
            true_false_map[rel[0][ttvar("x", i)]] for i in range(len(rel[0]))
        )
        clause = f"(f {clause})"
        if not rel[1]:  # case is false: negate it
            clause = f"(!{clause})"
        clauses.append(clause)
    return "assert (" + " && ".join(clauses) + ")\n"


def make_test_strings(truth_tables, funcs):
    asserts = map(make_assert, truth_tables)
    # convert from ttvar to string in the format that we want...
    pretty_vars = map(lambda x: f"{x.name}{x.indices[0]+1}", truth_tables[0].inputs)
    header = "let f " + " ".join(f"({var} : bool)" for var in pretty_vars) + " ="
    # make the actual strings
    strings = []
    for func, assert_ in zip(funcs, asserts):
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


def gen_curricula(funcs, ttables):
    curriculum = defaultdict(lambda: [])
    max_num_steps = 1
    for func, ttable in tqdm(zip(funcs, ttables), total=len(funcs)):
        # get our curriculum for that file, unzip into two lists
        test_funcs, cursor_starts = make_curriculum(Node.from_eda(func))
        # print(test_funcs, cursor_starts)
        max_num_steps = max(max_num_steps, len(test_funcs) + 1)
        # generate full test functions
        ttable_list = [copy(ttable) for _ in range(len(cursor_starts))]
        test_strings = make_test_strings(
            ttable_list, map(lambda x: x.to_ocaml(), test_funcs)
        )
        for test_str, cursor_pos in zip(test_strings, cursor_starts):
            curriculum[cursor_pos].append(test_str)
    return curriculum, max_num_steps


def save_curriculum(
    curriculum, targ_dir, max_num_steps, done_action=True, test_split=None
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
    ttables, pretty_funcs, funcs = make_nfuncs(args.n_args, args.simplify)
    if args.test_split: 
        folds = split_folds((ttables,pretty_funcs,funcs),args.targ_dir,args.test_split,args.seed)
    else: 
        folds = {args.targ_dir:(ttables,pretty_funcs,funcs)}

    for targ_dir, (ttables,pretty_funcs,funcs) in folds.items(): 
        if args.curriculum:
            curriculum, max_steps = gen_curricula(
                funcs,
                ttables,
                test_split=args.test_split,
                seed=args.seed,
            )
            save_curriculum(curriculum, targ_dir, max_steps)
        else:
            test_strings = make_test_strings(ttables, pretty_funcs)
            save_test_strings(test_strings, args.targ_dir)


if __name__ == "__main__":
    args = parse_args()
    main(args)