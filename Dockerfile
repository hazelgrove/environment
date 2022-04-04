# syntax=docker/dockerfile:1

FROM python:3.8
WORKDIR /RL_env

# Copy all files
COPY . .

# Install opam
RUN echo "\n" | bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
RUN opam init --yes --disable-sandboxing

# Install dependencies
RUN make deps

ENTRYPOINT [ "/RL_env/entrypoint.sh" ]