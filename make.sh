#!/bin/bash

rm -rf ucw-boxset ucw-boxset.tar ucw-boxset.tar.gz

darcs get --partial http://common-lisp.net/project/ucw/repos/ucw-boxset/

cd ucw-boxset
bash ./get-all.sh
cd ..

tar -zcf ucw-boxset.tar.gz ucw-boxset
scp ucw-boxset.tar.gz mbaringer@common-lisp.net:/project/ucw/public_html/
