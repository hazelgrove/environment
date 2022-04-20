# inspired by https://sourcery.ai/blog/python-docker/
FROM nvidia/cuda:11.6.0-base-ubuntu20.04 as base

ENV LC_ALL C.UTF-8

# no .pyc files
ENV PYTHONDONTWRITEBYTECODE 1

# traceback on segfau8t
ENV PYTHONFAULTHANDLER 1

# use ipdb for breakpoints
ENV PYTHONBREAKPOINT=ipdb.set_trace

# common dependencies
RUN apt-get update -q \
 && DEBIAN_FRONTEND="noninteractive" \
    apt-get install -yq \
      # git-state
      git \
      # primary interpreter
      python3.8 \
      # redis-python
      redis \
 && apt-get clean

FROM base AS deps
# TODO: This is probably where most of the logic from your current Dockerfile goes
# build dependencies
RUN apt-get update -q \
 && DEBIAN_FRONTEND="noninteractive" \
    apt-get install -yq \
      # required by poetry
      python3-pip \
      opam \
      gcc \
      cmake \
 && apt-get clean
WORKDIR "/deps"
COPY pyproject.toml poetry.lock opam.export /deps/
RUN pip3 install poetry && poetry install \
 && opam init --yes --disable-sandboxing \
 && opam install --yes \
 ocaml \
 dune \
 ocamlformat.0.20.1 \
 menhir \
 core \
 sexplib \
 ppx_sexp_conv \
 lwt
ENV PYTHON_ENV=/root/.cache/pypoetry/virtualenvs/hazelnut-K3BlsyQa-py3.8/

# Build OCaml code
WORKDIR "/env"
COPY ocamllib .
COPY clib .
RUN eval $(opam config env) \
 && make astenv
ENV OCAML_ENV=/root/.opam

FROM base AS runtime
WORKDIR "/RL_env"
ENV PATH="$VIRTUAL_ENV/bin:$PATH"
COPY --from=deps $PYTHON_ENV $PYTHON_ENV
COPY --from=deps $OCAML_ENV $OCAML_ENV
COPY --from=deps /env/_build/default/ocamllib/libcinterface.so /env/clib/astclib.so .
COPY . .
ENTRYPOINT ["/bin/bash"]