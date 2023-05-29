#!/usr/bin/env python

# imports 
from itertools import product       # forms cartesian products
import argparse
import random
import os 
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
    if len(elts) <= 1: 
        return  ''.join(elts)
    else:
        return f'({elts[0]} && ' +  and_string(elts[1:]) + ')'

def or_string(elts): 
    if len(elts) <= 1: 
        return  ''.join(elts)
    else:
        return f'({elts[0]} || ' +  or_string(elts[1:]) + ')'


# now make funcs ? 
def make_funcs(n,max_num = 400, print_funcs = False, print_asserts = False): 
    variables = [f'x{i}' for i in range(n)]
    pairs = [('(!'+var + ')', var) for var in variables]

    build_assert = lambda inpts,outpt: '(f ' + ' '.join(inpts) + ') = '+ str(outpt)

    inputs = list(product(*pairs))
    xvals = list(product(['0','1'],repeat = n))
    functs,assert_strs = [],[]
    for i, outputs in enumerate(product([0, 1], repeat=len(inputs))):
        if i >= max_num: 
            break 
        terms = [and_string(row) for row, output in zip(inputs, outputs) if output]
        funct = or_string(terms)[1:-1]
        if not terms:
            funct = 'False'
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


# In[14]:

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
    parser.add_argument("--n",type=int,default=3, help="number of inputs")
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

	
def main(): 
    args = parse_args()
    funct_list, assert_list = make_funcs(args.n,args.max_funcs)
    tests = constr_test_cases(args.n,assert_list)

    # now partition and save 
    random.seed(args.seed)
    tests_shuff = tests
    random.shuffle(tests_shuff)

    n_train = int((1-args.test_ratio)*float(len(tests)))
    train, test = tests[:n_train], tests[n_train:]
    # save 
    train_dir = os.path.join(args.outdir,'train','0')
    test_dir  = os.path.join(args.outdir,'test' ,'0')

    save_tests(train,train_dir)
    save_tests(test,test_dir)


if __name__ == "__main__":
	main()


