# this is a script that generates a number of test cases. 
from textwrap import dedent 
import random 
import os
from itertools import product
from typing import List,Tuple
from collections import defaultdict
from shutil import rmtree
from argparse import ArgumentParser

prefix = dedent("""\
	let f (x1: int) = 
		\
	""")
postfix =dedent("""\

	in 
	assert(f 0 = {})
	""")

out_dir = "data/generated_tests/binary_tests/"

def make_test_schedule(targ_val, num_steps,num_ones = None ,seed=43):
	random.seed(seed)
	max_num_ones =  (num_steps+ targ_val)//2
	if num_ones is None: 
		num_ones = random.randint(0,max_num_ones)
	num_neg_ones = num_ones - targ_val
	num_zeros = num_steps - (num_ones + num_neg_ones) 
	num_neg_zeros = random.randint(0,num_zeros)
	# print(f'targ: {targ_val: 1d} n_steps: {num_steps} max_ones:{max_num_ones}, num_ones: {num_ones}')

	if num_zeros <0 : 
		assert(False) # ERROR 
	assert(num_ones -num_neg_ones == targ_val)
	schedule = ['-1']*num_neg_ones + ['1']*num_ones + ['0']*(num_zeros-num_neg_zeros) + ['0']*(num_neg_zeros)
	# print(schedule)
	random.shuffle(schedule)
	return schedule

def make_test_case(schedule,repl_zero_rate=0.2,seed=None): 
	if seed is not None: 
		random.seed(seed)
	target = sum(int(elt.replace(' ','')) for elt in schedule)
	has_zeros = any('0' in elt for elt in schedule)
	has_ones = any('1' in elt for elt in schedule)

	# print(target)
	test = "" 
	test += prefix

	# prepare schedule to join as string 
	if "+" in schedule[0]: 
		print(schedule[0])
		schedule[0] = schedule[0].replace('+', "")
		print(schedule[0])

	if '0' in schedule[0]: schedule[0] = '0'
	# choose item to mask. right now always choose random nonzero elt  
	indxs = list(range(len(schedule)))
	random.shuffle(schedule)
	hole_pos = None 
	if (random.random() < repl_zero_rate or not has_ones) and has_zeros:
		for i in indxs: 
			if '0' in schedule[i]: 
				schedule[i] = '?' # schedule[i].replace('0','?')
				hole_pos = i 
				break;

	else: 
		for i in indxs: 
			if '1' in schedule[i]: 
				schedule[i] = '?' #schedule[i].replace('1','?')
				hole_pos = i 
				break; 
	test += ' + '.join(schedule) 
	test += postfix.format(target)
	hole_pos = max(1,2*hole_pos)
	return test,target,hole_pos

def save_tests(tests:List[Tuple[str,int,int]],
              fold:str,
              out_dir = "data/generated_tests/binary_tests_short/",
              out_desc_name = f'out_desc_lists.yaml',
			):
	basedir = os.path.join(out_dir,fold)

	spots = defaultdict(lambda :[])
	for test,num_steps,hole_pos in tests: 
		spots[hole_pos].append(test)

	spots_list = []
	num_elts_list = []
	for i, (hole_pos, tests) in enumerate(spots.items()):
		targ_dir = os.path.join(basedir,str(i))
		# clear target directory
		if os.path.exists(targ_dir):
			rmtree(targ_dir)
		os.makedirs(targ_dir)
		with open(os.path.join(targ_dir,'test.ml'),'w') as f :
			f.write('[]\n')
		# update our labels
		spots_list.append(hole_pos)
		num_elts_list.append(len(tests))
		# write our files 
		for elt, test in enumerate(tests): 
			with open(os.path.join(targ_dir,f"{elt}.ml"),'w') as file: 
				file.write(test)
		
    
    # print output label file 
	with open(os.path.join(basedir,out_desc_name),'w') as file:
		file.write(f'  assignment_dir: {basedir}\n') # remove '../'
		file.write(f'  num_assignments: {len(spots_list)}\n')
		file.write(f'  code_per_assignment: {num_elts_list}\n')
		file.write(f'  cursor_start_pos: {spots_list}\n')



def main():
	parser = ArgumentParser()
	parser.add_argument("--min_num_steps",default=2,help="min tuple length",required=False)
	parser.add_argument("--max_num_steps",default=6,help="max tuple length",required=False )
	parser.add_argument("--max_tests_per",default=30,help="max number of tests per type, incl double counts",required=False)
	parser.add_argument("--max_num_ones",default=999,help="max number of ones, set to small to achieve zero-heavy tests",required=False)
	parser.add_argument("--test_ratio",default=0.3,help="percentage of examples to save for test set",required=False)
	parser.add_argument("--outdir",default="data/generated_tests/binary_tests/",help='target path',required=False)
	args = parser.parse_args() 

	targs = [-1,0,1]
	num_steps = list(range(int(args.min_num_steps),int(args.max_num_steps)+1))
	test_split = {2:0.4,3:0.3} 
	test_split.update({val:float(args.test_ratio) for val in num_steps if val not in test_split.keys()})
	
	num_tests_per = int(args.max_tests_per) # generate all possible teests, more or less
	max_num_ones = int(args.max_num_ones) # do not cap
	i=0
	schedule_strs = set()
	schedules = {step:[] for step in num_steps}
	for num_steps,targ in product(num_steps,targs):
		
		max_ones =  (num_steps+ targ)//2
		for test_num in range(num_tests_per):
			for chosen_num_ones in range(1,min(max_ones,max_num_ones)+1):
				schedule = make_test_schedule(targ,num_steps,chosen_num_ones,seed=i*2)
				schedule_str = str(schedule)
				if schedule_str not in schedule_strs:
					# ensure we dont have duplicates
					schedule_strs.add(schedule_str)
					# print(schedule)
					test,target,hole_pos = make_test_case(schedule,seed=i)
					schedules[num_steps].append((test,num_steps,hole_pos))
					i +=1 
	
	test_schedules  = []
	train_schedules = []
	for step,sched_list in schedules.items():
		random.seed(42)
		if step not in test_split.keys(): 
			continue
		n_test = int(len(sched_list)*test_split[step])
		random.shuffle(sched_list)
		train_schedules.extend(sched_list[n_test:]) 
		test_schedules.extend(sched_list[:n_test]) 
		print(f'added {n_test:2d} elts to test schedules ({len(sched_list[n_test:]):2d} left over) with {step} steps.')
	save_tests(test_schedules,out_dir=args.outdir,fold='test')
	save_tests(train_schedules,out_dir=args.outdir,fold='train')



	



if __name__ == "__main__":
	main()