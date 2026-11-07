# Builds the native Darklang compiler (pbiggar/darklang-compiler) at a pinned commit, with
# the toolchain that repo pins (OCaml 5.5.1, its dependencies.lock). Used by
# scripts/compiler/build; the result runs in the dev container (same Ubuntu 24.04).
FROM ubuntu:24.04 AS ocaml
RUN apt-get update && apt-get install -y --no-install-recommends build-essential ca-certificates curl
WORKDIR /ocaml-build
RUN curl -fL https://github.com/ocaml/ocaml/archive/refs/tags/5.5.1.tar.gz -o ocaml.tar.gz \
 && echo "ae47728d0a876665ccc7b745e3a8d8c9ca5ba63c4f7a11d4db6cdbd1dbeef4bb  ocaml.tar.gz" | sha256sum --check \
 && tar xzf ocaml.tar.gz --strip-components=1 \
 && ./configure --prefix=/opt/ocaml && make -j"$(nproc)" world.opt && make install

FROM ubuntu:24.04
RUN apt-get update && apt-get install -y --no-install-recommends \
      build-essential ca-certificates curl git libgmp-dev libsqlite3-dev pkg-config opam python3 unzip
COPY --from=ocaml /opt/ocaml /opt/ocaml
ENV PATH=/opt/ocaml/bin:$PATH
RUN opam init --bare --disable-sandboxing --yes && opam switch create dark ocaml-system.5.5.1 --yes
ARG COMPILER_COMMIT
RUN git clone https://github.com/pbiggar/darklang-compiler.git /src && git -C /src checkout --quiet "$COMPILER_COMMIT"
RUN sed '/^[[:space:]]*#/d; /^[[:space:]]*$/d' /src/dependencies.lock \
      | xargs opam install --switch=dark --yes --jobs="$(nproc)" --no-depexts
RUN eval "$(opam env --switch=dark)" && dune build --root /src bin/dark.exe
