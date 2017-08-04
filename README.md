[![Build Status](https://travis-ci.org/xenserver/upload-scripts.svg?branch=master)](https://travis-ci.org/xenserver/upload-scripts)
[![Lines of Code](https://tokei.rs/b1/github/xenserver/upload-scripts)](https://github.com/xenserver/upload-scripts)

Adding missing packages
-------------

1. Add any missing component to `SOURCES` in `generate-whitelist.sh`.
2. Run `generate-whitelist.sh` from within a planex-buildenv container of the relevant product branch.
3. Add any missing package to `whitelist` in `src/update_xs_yum.ml`.
