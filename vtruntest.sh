#! /bin/sh
# Copyright (c) 2008,2009 Meikel Brandmeyer, Frankfurt am Main
# 
# All rights reserved.
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
# 
# Description:
# This is a small script coming with VimTAP in order to run the
# tests from outside of vim.
#

if [ $# -ne 1 ]; then
	echo "Usage: vtruntest.sh <test>"
	exit 1
fi

test=$1
testoutput=`mktemp -t vimtap`

# XXX: In the following there is no need to "catch" the BailOut exception,
# since vim reads from stdin. That means that each command comprises and own
# "run", which is isolated from the others. If we would put the commands in
# separate file and :source'd it, we would have needed the "catch".
vim -E <<EOF
call vimtap#SetOutputFile("${testoutput}")
source ${test}
call vimtap#FlushOutput()
qa!
EOF

cat ${testoutput}

rm -f ${testoutput}

