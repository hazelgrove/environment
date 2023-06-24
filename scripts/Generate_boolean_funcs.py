#!/usr/bin/env python

# imports 
from itertools import product       # forms cartesian products
import argparse
import random
import os 
import json
from shutil import rmtree


def make_truth_tables(n, print_tts=True): 
    if print_tts: 
        print('All possible truth tables for n =', n)
    
    inputs = list(product([0, 1], repeat=n))
    mappings = list(product([0, 1], repeat=len(inputs)))
    if print_tts: 
        for i,output in enumerate(mappings):
            print()
            print(f'Truth table for f{i}:')
            print('-----------')
            for row, result in zip(inputs, output):
                print(row, '-->', result)
    print(len(mappings))
    return mappings


def and_string(elts): 
    return '(' + " && ".join(elts) + ')'

def or_string(elts): 
    return '(' + " || ".join(elts) + ')'


# now make funcs ? 
def make_funcs(n,max_num = 400, print_funcs = False, print_asserts = False): 
    variables = [f'x{i}' for i in range(n)]
    pairs = [('(!'+var + ')', var) for var in variables]

    neg_if_needed = lambda stmt, neg: f'(!{stmt})' if neg else stmt
    build_assert = lambda inpts,output:  neg_if_needed('(f ' + ' '.join(inpts) + ')',not output)

    inputs = list(product(*pairs))
    xvals = list(product(['false','true'],repeat = n))
    functs,assert_strs = [],[]
    for i, outputs in enumerate(product([0, 1], repeat=len(inputs))):
        if i >= max_num: 
            break 
        terms = [and_string(row) for row, output in zip(inputs, outputs) if output]
        funct = or_string(terms)[1:-1]
        if not terms:
            funct = 'false'
        truth_table = [build_assert(xval,yval) for xval, yval in zip(xvals,outputs)]
        truth_table = [build_assert(xval,yval) for xval, yval in zip(xvals,outputs)]
        assert_str = and_string(truth_table)
        if print_funcs: 
            print('Function %d:' % i,funct)
        if print_asserts:
            print(assert_str)

        functs.append(funct)
        assert_strs.append(assert_str)
        
        
    return functs, assert_strs

# now construct the actual assert 
def constr_test_cases(n,asserts):
    cases = []
    preamble = "let f " + ' '.join(f'( x{i+1} : bool )' for i in range(n))  + '= \n'
    funct = '\t?\n'
    for asst in asserts: 
        end = 'in \n' + 'assert ' + asst + '\n'
        cases.append(preamble + funct + end)
    return cases
    
def constr_solution(): 
    return None 


def parse_args(): 
    parser = argparse.ArgumentParser()
    parser.add_argument("--min_n",type=int,default=1, help="number of inputs")
    parser.add_argument("--max_n",type=int,default=3, help="number of inputs")
    parser.add_argument('--choose_tests', type=str,default=None,help='if seleccted must be a json list of file numbers to take. Checks that shuffle is off, only one n is given, and no test files are generated.')
    parser.add_argument("--shuffle",type=bool,default=True,help='whether to shuffle tests')
    parser.add_argument("--max_funcs",type=int,default =500, help= "the maximum number of tests to generate")
    parser.add_argument("--test_ratio",type=float,default =0.2, help= "percentage of functions to reserve for test set")
    parser.add_argument("--outdir",type=str,default ="data/generated_tests/binary_funcs", help= "where to store the files")
    parser.add_argument("--seed",default=42)
        
    return parser.parse_args()

def save_tests(tests,targdir):
    # prepare directory (clear and reprepare)
    if os.path.exists(targdir):
        rmtree(targdir)
    os.makedirs(targdir,exist_ok=True)
    # then write vestigial file  
    with open(os.path.join(targdir,'test.ml'),'w') as f :
        f.write('[]\n')
    # finally, actually write our tests 
    for i, test in enumerate(tests): 
       with open(os.path.join(targdir,f'{i}.ml'),'w') as file: 
            file.write(test)
    print(f'saved {len(tests)} files to "{targdir}"')

def make_func_batch(n_inputs,outdir,test_ratio = 0.2,seed=42,shuffle=True, save_num = 0, max_funcs=400,choose_tests=None): 
    funct_list, assert_list = make_funcs(n_inputs,max_funcs)
    tests = constr_test_cases(n_inputs,assert_list)
    # now partition and save 

    tests_shuff = tests
    print(choose_tests,len(tests_shuff))
    if choose_tests:
        tests_shuff = [elt for i,elt in enumerate(tests_shuff) if i in choose_tests]
    print(choose_tests,len(tests_shuff))

    if shuffle:
        random.seed(seed)
        random.shuffle(tests_shuff)

    n_train = int((1-test_ratio)*float(len(tests_shuff)))
    train, test = tests_shuff[:n_train], tests_shuff[n_train:]
    # save 
    train_dir = os.path.join(outdir,'train',str(save_num))
    test_dir  = os.path.join(outdir,'test' ,str(save_num))
    save_tests(train,train_dir)
    save_tests(test,test_dir)

def clean_tests(outdir): 
    if os.path.exists(os.path.join(outdir,'train')):
        rmtree(os.path.join(outdir,'train'))
    if os.path.exists(os.path.join(outdir,'test')):
        rmtree(os.path.join(outdir,'test'))
	
def main(): 
    args = parse_args()

    clean_tests(args.outdir)
    if args.choose_tests is not None: 
        choose_tests = json.loads(args.choose_tests)
        choose_tests = [ [int(elt) for elt in lst] for lst in choose_tests]
        assert(len(choose_tests) == (args.max_n - args.min_n + 1))

    else: 
        choose_tests = None
        
    for i, n in enumerate(range(args.min_n,args.max_n+1)):
        make_func_batch(
            n_inputs=n,
            outdir=args.outdir,
            test_ratio =args.test_ratio,
            seed=args.seed,
            save_num=i, 
            max_funcs= args.max_funcs,
            shuffle=args.shuffle,
            choose_tests=choose_tests[i]
        )


if __name__ == "__main__":
	main()


