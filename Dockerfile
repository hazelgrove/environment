# syntax=docker/dockerfile:1

FROM python:3.8
WORKDIR /RL_env
COPY requirements.txt requirements.txt
RUN pip install -r requirements.txt
COPY . .

CMD ["python3", "/RL_env/test.py"]