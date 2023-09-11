import openai
import time
import pandas as pd
import subprocess
import re 
import os
from tqdm import tqdm
from itertools import product
from codex_api_key import API_KEY


def get_completion(prompt,assrt,max_tokens=20,temperature=1.0):
    if assrt: 
        function = f'{prompt}\n    ?\n{assrt}'
    else: 
        function=prompt
    messages = [{'role':'user','content':function},
                {'role':'user','content':'Replace the question mark in the above ocaml function with ocaml code that passes the assert.' }]

    jsonobj = {
        "model": "gpt-3.5-turbo", 
        # "prefix": "(* ocaml code *)",
        'messages':messages,
        "temperature": temperature,
        "max_tokens": max_tokens
    }
    resp = openai.ChatCompletion.create(
        **jsonobj
    )
    insertion = resp['choices'][0]['message']['content'] if 'choices' in resp.keys() else None
    if insertion:
        return insertion, resp
    return (None, resp)

def get_question_edit(prompt,assert_=None, max_tokens=20,temperature=1.0):
    if assert_: 
        function = f'{prompt}\n    ?\n{assert_}'
    else: 
        function=prompt

    instruction= 'Replace the question mark in the above ocaml function with ocaml code that passes the assert.' 
    model = "code-davinci-edit-001"

    resp = openai.Edit.create(model=model,
                                  input=function,
                                  instruction=instruction,
                                  temperature=temperature)
    insertion = resp['choices'][0]['text'] if 'choices' in resp.keys() else None
    if insertion:
        return insertion, resp
    return (None, resp)

def test_input(text,assert_,use_temp_file='temp.txt'):
    test_text = text + '\n' + assert_
    with open(use_temp_file, 'w') as file:
        file.write(test_text)
    prc = subprocess.run(['ocaml','temp.txt'],stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL )
    return prc.returncode == 0

def make_log(test,response,accurate, mode,test_n,failed=False): 
    log_json = {
        'test_n':test_n,
        'test' : test, 
        'accurate':accurate,
        'mode':mode,
        'failed':failed,
        'response': response,
    }
    return pd.Series(log_json) 
    

if __name__ == "__main__": 
    openai.api_key = API_KEY

    n_samples = 50 
    counts = 0 
    corrects = 0 
    logs = []
    

    # do gpt 3.5 / 'completion' task 
    for _,file_n in (bar := tqdm(list(product(range(n_samples),os.scandir('data/blank_2var_tests'))))): 
        if re.match(r'.*\.ml',file_n.name): 
            with open(file_n.path,'r') as file: 
                test = file.read()
                test = test.replace('!', 'not') 
            res, json = get_completion(test,None,max_tokens=100)
            if res: 
                assert_stmt = test.split('?')[1]
                func_body = res.split('in')[0]
                # print(func_body + assert_stmt)
                correct =  test_input(func_body,assert_stmt)
                if correct:
                    corrects +=1
                counts +=1 
                logs.append(make_log(test,func_body,correct,'completion',file_n.name))
            else: 
                logs.append(make_log(test,None,False,'completion',file_n.name,failed=True))
                
        chat_success_rate = corrects/counts if counts != 0 else 0.0 
        bar.set_description(f'success_rate = {chat_success_rate:1.2f}')
        bar.bar_format = '{l_bar}{bar}{r_bar}' + f'file={file_n.name}'

    print(f'Chat completion format score = {chat_success_rate}')
    
    counts = 0 
    corrects = 0 
    min_time_per_cycle = 3.001
    begin_time = time.time()

    for _,file_n in (bar := tqdm(list(product(range(n_samples),os.scandir('data/blank_2var_tests'))))): 
        if re.match(r'.*\.ml',file_n.name): 
            # the other (non-depricated endpoint) handles rate limits. This one does not. 
            # Therefore we need to handle rate limits ourselves. 
            # handle rate limit stuff (albeit jankily)
            end_time = time.time()
            if (elapsed_time := end_time - begin_time) < min_time_per_cycle:
                # enforce min of two seconds per cycle to avoid rate limit 
                time.sleep(min_time_per_cycle - elapsed_time)
            begin_time = time.time()
            # process request 
            with open(file_n.path,'r') as file: 
                test = file.read()
                test = test.replace('!', 'not') 
            res, json = get_question_edit(test,None,max_tokens=100)
            if res: 
                assert_stmt = test.split('?')[1]
                func_body = res.split('in')[0]
                
                # check correctness and update counts
                correct =  test_input(func_body,assert_stmt)
                if correct:
                    corrects +=1
                counts +=1 
                logs.append(make_log(test,func_body,correct,'edit',file_n.name))
            else: 
                logs.append(make_log(test,None,False,'edit',file_n.name,failed=True))

        edit_success_rate = corrects/counts if counts != 0 else 0.0 
        bar.set_description(f'success_rate = {edit_success_rate:1.2f}')
        bar.bar_format = '{l_bar}{bar}{r_bar}' + f'file={file_n.name}'

    print(f'Chat completion format score = {edit_success_rate}')
    with open('data/coarse_logs.txt','w') as file: 
        file.write('mode, accuracy\n')
        file.write(f'chat, {chat_success_rate}\n')
        file.write(f'edit, {edit_success_rate}\n')

    fine_logs = pd.DataFrame(logs)
    print(fine_logs)
    with open('data/fine_logs.csv','w') as file: 
        fine_logs.to_csv(file)
    with open('data/fine_logs.json','w') as file: 
        fine_logs.to_json(file)
    with open('data/fine_logs.pikl','wb') as file: 
        fine_logs.to_pickle(file)