# syntax=docker/dockerfile:1

FROM python:3.8
WORKDIR /RL_env

# Install Python dependencies
COPY requirements.txt requirements.txt
RUN pip install -r requirements.txt

# Install OCaml dependencies
RUN apt update
RUN apt install opam --yes
RUN opam init --yes --disable-sandboxing
RUN opam install dune --yes
RUN opam install menhir --yes
RUN opam install ounit2 --yes
RUN opam install ocamlformat.0.20.1 --yes
RUN opam install ppx_sexp_conv --yes
RUN opam install sexplib --yes
RUN opam install core --yes

# Copy all files
COPY . .


ENTRYPOINT [ "/RL_env/entrypoint.sh" ]