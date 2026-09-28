#!/usr/bin/env sh
# Vendored repos must not pull in their own vendor/ submodules (that would
# duplicate libraries in the dune workspace). Delete their .gitmodules so
# `git submodule update` never fetches them.
set -e

find src vendor -name .gitmodules -type f -delete
