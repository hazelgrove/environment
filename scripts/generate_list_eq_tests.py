# this is a script that generates a number of test cases. 
from textwrap import dedent 
import random 
import os
from itertools import product
from typing import List,Tuple
from collections import defaultdict
from shutil import rmtree
import argparse


prefix = dedent("""\
	let f (x1: int) = 
		\
	""")
postfix =dedent("""\

	in 
	assert( equal ({}) (f 0))
	""")

out_dir = "data/generated_tests/list_tests/"
out_path = 'test_01'

def make_test_schedule(min_val, max_val,len ,seed=43):
	random.seed(seed)
	schedule = [str(random.randint(min_val,max_val)) for _  in range(len)]
	return schedule

def make_test_case(schedule,repl_zero_rate=0.2,seed=None): 
	if seed is not None: 
		random.seed(seed)

	# print(target)
	test = "" 
	test += prefix

	#sol string 
	sol_string = "( " + "::".join(schedule) + ":: [] )"

	# place hole and make assignment 
	hole_pos = random.randint(0,len(schedule)-1)
	schedule[hole_pos] = "?"
	assn_string =  "( " + "::".join(schedule) + ":: [] )\n"

	test = prefix + assn_string  + postfix.format(sol_string) 

	return test,hole_pos

def save_tests(tests:List[Tuple[str,int,int]],
              fold:str,
              out_dir = "data/generated_tests/list_tests/",
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
	min_val,max_val = -1,4
	min_num_steps, max_num_steps = 2,5
	num_steps = list(range(min_num_steps,max_num_steps+1))
	test_split = {
			2:0.4,
			3:0.3, 
			4:0.3, 
			5:0.3, 
			# 6:0.2,
			# 7:0.1
		} 
	num_tests_per = 200 # generate all possible teests, more or less
	i=0
	schedule_strs = set()
	schedules = {step:[] for step in num_steps}
	for num_steps in num_steps:
		for test_num in range(num_tests_per):
			schedule = make_test_schedule(min_val,max_val,num_steps,seed=i*2)
			schedule_str = str(schedule)
			if schedule_str not in schedule_strs:
				# ensure we dont have duplicates
				schedule_strs.add(schedule_str)
				# print(schedule)
				test,hole_pos = make_test_case(schedule,seed=i)
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
	save_tests(test_schedules,fold='test')
	save_tests(train_schedules,fold='train')

def parse_args(): 
	parser = argparse.ArgumentParser()
	parser.add_argument("min_val",type=int,default=-1, help="the min integer to allow")
	parser.add_argument("max_val",type=int,default =4, help= "the maximum integer to allow ")
	parser.add_argument("max_val",type=int,default =4, help= "the maximum integer to allow ")
	parser.add_argument("max_val",type=int,default =4, help= "the maximum integer to allow ")

	arg_dict = {}

	



if __name__ == "__main__":
	main()