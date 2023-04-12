import yaml
import subprocess
import re

def generate_tests(config_path="params.yaml"):
    with open(config_path, "r") as file:
        params = yaml.safe_load(file)
    gen_params = params["make_tests"]
    call_cmd = gen_params['command']
    for key in gen_params.keys():
        # allow multiple callings of same script
        # use 
        print(key)

        if re.match(r'arg(s?)\d*',key):
            args = gen_params[key]
            call_params = re.split(r'\s',call_cmd)
            if args is not None:
                for key2,value in args.items(): 
                    call_params.append(f'--{key2}')
                    if value is not None:
                        call_params.append(str(value))
            print(call_params)
            out = subprocess.call(call_params)
    
    
if __name__=="__main__": 
    generate_tests()
    