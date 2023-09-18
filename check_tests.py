import ctypes
import yaml
import pkgutil
import os
print(os.path.abspath(os.path.curdir))
from envs.ast_env import State
from agent.arguments import read_params

def init():
	max_num_nodes=150
	num_node_descriptor=107
    # num_assignments=num_assignments,
    # code_per_assignment=code_per_assignment,
	num_actions=132
	max_num_tests = 1000
	max_num_vars = 10
	max_tree_length = 10000
	cursor_start_pos=0
	State._fields_ = [
		("edges", (ctypes.c_int * (max_num_nodes * 3)) * 3),
		("tests", (ctypes.c_int * max_num_tests) * 2),
		("nodes", ctypes.c_int * max_num_nodes),
		("starter", ctypes.c_int * max_num_nodes),
		("permitted_actions", ctypes.c_int * (num_actions + max_num_vars * 2)),
		("vars_in_scope", ctypes.c_int * max_num_vars),
		("args_in_scope", ctypes.c_int * max_num_vars * 2),
		("zast", ctypes.c_char * max_tree_length),
		("cursor", ctypes.c_int),
		("num_nodes", ctypes.c_int),
		("num_edges", ctypes.c_int),
		("num_tests", ctypes.c_int),
		("num_vars", ctypes.c_int),
		("num_args", ctypes.c_int),
		("assignment", ctypes.c_int),
		("code", ctypes.c_int),
	]

def check_test(test_dir,assignment_num,test_num): 

	astclib = ctypes.CDLL("/RL_env/clib/astclib.so")

	state = State()
	correct = False
	try: 
		astclib.init_assignment(
					ctypes.byref(state),
					bytes(test_dir, encoding="utf8"),
					ctypes.c_int(assignment_num),
					ctypes.c_int(test_num),
					ctypes.c_int(True), # perturbation
					ctypes.c_int(-1),
				)
		correct = True 
	except :
		correct = False 

	return correct 

def test_dir(param_file):
	init()
	print(f'testing dir with param file {param_file}')
    # params = read_params(param_file)

	file = params['env']['assignment_dir']
	print(params['env']['num_assignments'])
	for assignment in range(params['env']['num_assignments']):
		print(params['env']['code_per_assignment'][assignment])
		for test in range(params['env']['code_per_assignment'][assignment]):
			print(f'testing {assignment},{test}')
			if not check_test(file,assignment,test):
				print(f'Error in asssn:{assignment}, test:{test}')
			else: 
				print(f'success; asssn:{assignment}, test:{test}')


if __name__ == "__main__":
	params = 'params.yaml'
	test_dir(params)
	print(list(pkgutil.iter_modules()))