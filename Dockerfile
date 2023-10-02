# inspired by https://sourcery.ai/blog/python-docker/
FROM nvidia/cuda:11.5.2-devel-ubuntu20.04 as base

ENV LC_ALL C.UTF-8

# no .pyc files
ENV PYTHONDONTWRITEBYTECODE 1

# traceback on segfau8t
ENV PYTHONFAULTHANDLER 1

# use ipdb for breakpoints
ENV PYTHONBREAKPOINT=ipdb.set_trace

# common dependencies
RUN apt-key adv --fetch-keys https://developer.download.nvidia.com/compute/cuda/repos/ubuntu2004/x86_64/3bf863cc.pub \
  && apt-get update -q \
  && DEBIAN_FRONTEND="noninteractive" \
  apt-get install -yq \
  # git-state
  git \
  # primary interpreter
  python3.9 \
  # redis-python
  redis \
  cmake \
  python3-opencv \
  gcc \
  && apt-get clean

FROM base AS deps
RUN apt-get update -q \
  && DEBIAN_FRONTEND="noninteractive" \
  apt-get install -yq \
    python3.9 \
    python3-pip \
    cmake \
  && apt-get clean
WORKDIR "/deps"
COPY pyproject.toml poetry.lock requirements.txt /deps/
RUN python3.9 -m pip install -r requirements.txt 

ENV PYTHON_ENV=/usr/local/lib/python3.9/dist-packages

FROM base AS runtime
COPY --from=deps $PYTHON_ENV $PYTHON_ENV
WORKDIR "/RL_env"
COPY Graphormer Graphormer
RUN python3 -m pip install --upgrade pip
WORKDIR "/RL_env/Graphormer"
RUN ./install.sh
RUN apt-get update -q \
  && DEBIAN_FRONTEND="noninteractive" \
  apt-get install -yq \
  opam \
  && apt-get clean
COPY opam.export .
RUN opam init --yes --disable-sandboxing \
  && opam update \
  && opam switch create . ocaml-base-compiler.4.13.1 \
  && opam switch import opam.export --yes
ENV PATH="$PYTHON_ENV/bin:$PATH"
COPY . .

ENTRYPOINT ["/RL_env/entrypoint.sh"]
