"##### HEADER [ {{{ ]
" Plugin:       VimTAP
" Version:      0.3
" Author:       Meikel Brandmeyer <mb@kotka.de>
" Created:      Sat Apr 12 20:53:41 2008
"
" License:
" Copyright (c) 2008,2009 Meikel Brandmeyer, Frankfurt am Main
" 
" All rights reserved.
" 
" Permission is hereby granted, free of charge, to any person obtaining a copy
" of this software and associated documentation files (the "Software"), to deal
" in the Software without restriction, including without limitation the rights
" to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
" copies of the Software, and to permit persons to whom the Software is
" furnished to do so, subject to the following conditions:
" 
" The above copyright notice and this permission notice shall be included in
" all copies or substantial portions of the Software.
" 
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
" OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
" THE SOFTWARE.
"
" Description:
" VimTAP is an implementation of the Test Anything Protocol for vim. It is
" intended to assist the developer with testing his scripts. TAP makes it easy
" to test a project with different languages using a common harness to
" interpret the test results.
"
" See Also:
" http://search.cpan.org/~petdance/TAP-1.00/TAP.pm
" http://testanything.org
"##### [ }}} ]

"##### PROLOG [ {{{ ]
let s:saved_cpo = &cpo
set cpo&vim
"##### [ }}} ]

"##### VARIABLES [ {{{ ]
"### VARIABLE g:vimtap#TheHarness [ {{{ ]
" Description:
" The harness contains any state, which is necessary to run the tests.
" This is for example the test counter or the todo number.
"
" It is basically a dictionary enclosing all necessary information and
" providing methods for the state changing functions. That are:
"
"  * Plan
"  * Ok
"  * Diag
"  * Skip
"  * Todo
"
let vimtap#TheHarness = {}
"### [ }}} ]
"##### [ }}} ]

"##### CLASSES [ {{{ ]
"### CLASS vimtap#StandardHarness [ {{{ ]
" Description:
" A Harness collects the test results and further processes them. Any
" output be it results or diagnostics is handled by the harness. The
" global harness might be replaced temporarily to allow nesting subtests.
" The batch harness may then be inquired for the test results.
"
" Source:
let vimtap#StandardHarness = {}

"## METHODS [ {{{ ]
"# CLASS METHOD .New [ {{{ ]
function! vimtap#StandardHarness.New(outfname) dict
	let harness = copy(self)
	call self.Init(harness, a:outfname)
	return harness
endfunction
"# [ }}} ]
"# CLASS METHOD .Init [ {{{ ]
function! vimtap#StandardHarness.Init(self, outfname) dict
	let a:self.planned_tests = 0
	let a:self.test = 0
	let a:self.test_failed = 0
	let a:self.todos = 0
	let a:self.output = []
	let a:self.outfilename = a:outfname
endfunction
"# [ }}} ]
"# METHOD .plan [ {{{ ]
function! vimtap#StandardHarness.plan(tests) dict
	let self.test = 1
	let self.planned_tests = a:tests
	call self.print("1..%d", a:tests)
endfunction
"# [ }}} ]
"# METHOD .ok [ {{{ ]
function! vimtap#StandardHarness.ok(test_result, description) dict
	let result = a:test_result ? "ok" : "not ok"
	let self.test_failed = (a:test_result || self.todos > 0)
				\ ? self.test_failed : 1

	if self.todos > 0
		let desc = printf("# TODO %s", a:description)
		let self.todos = self.todos - 1
	else
		let desc = printf("- %s", a:description)
	endif

	call self.print("%s %d %s", result, self.test, desc)

	let self.test = self.test + 1
endfunction
"# [ }}} ]
"# METHOD .diag [ {{{ ]
function! vimtap#StandardHarness.diag(...) dict
	if a:0 == 1
		let message = a:1
	else
		let message = call(function("printf"), a:000)
	endif
	for line in split(message, '\(\r\n\|\r\|\n\)', 1)
		call self.print("# %s", line)
	endfor
endfunction
"# [ }}} ]
"# METHOD .skip [ {{{ ]
function! vimtap#StandardHarness.skip(num_tests, guard, reason) dict
	let nt = a:num_tests
	if !a:guard
		while nt > 0
			call self.print("ok %d # SKIP %s", self.test, a:reason)
			let self.test = self.test + 1
			let nt = nt - 1
		endwhile
	endif
	return !a:guard
endfunction
"# [ }}} ]
"# METHOD .todo [ {{{ ]
function! vimtap#StandardHarness.todo(num_tests) dict
	let self.todos = a:num_tests
endfunction
"# [ }}} ]
"# METHOD .bail_out [ {{{ ]
function! vimtap#StandardHarness.bail_out(reason) dict
	call self.print("\nBail out! %s", a:reason)
	throw "VimTAP:BailOut:" . a:reason
endfunction
"# [ }}} ]
"# METHOD .print [ {{{ ]
function! vimtap#StandardHarness.print(...) dict
	if a:0 == 1
		let output = a:1
	else
		let output = call(function("printf"), a:000)
	endif
	let self.output += split(output, '\n', 1)
endfunction
"# [ }}} ]
"# METHOD .flush [ {{{ ]
function! vimtap#StandardHarness.flush() dict
	if self.outfilename != ''
		call writefile(self.output, self.outfilename)
	else
		call append(line("$"), self.output)
		1 delete
	endif
endfunction
"# [ }}} ]
"## [ }}} ]

" Now create the initial default context.
let vimtap#TheHarness = vimtap#StandardHarness.New('')
"### [ }}} ]
"### CLASS vimtap#StandardHarness [ {{{ ]
" Description:
" The batch harness simply reduces the test results to a a single
" pass/fail answer and collects the diagnostic information.
"
" The result might be retrieved via .test_failed. The diagnostic
" output is available via .diagnostics.
"
" Source:
let vimtap#BatchHarness = copy(vimtap#StandardHarness)

"## METHODS [ {{{ ]
"# CLASS METHOD .New [ {{{ ]
function! vimtap#BatchHarness.New() dict
	let harness = copy(self)
	call self.Init(harness)
	return harness
endfunction
"# [ }}} ]
"# CLASS METHOD .Init [ {{{ ]
function! vimtap#BatchHarness.Init(self) dict
	call g:vimtap#StandardHarness.Init(a:self, '')
	let a:self.diagnostics = []
endfunction
"# [ }}} ]
"# METHOD .diag [ {{{ ]
function! vimtap#BatchHarness.diag(...) dict
	if a:0 == 1
		let output = a:1
	else
		let output = call(function("printf"), a:000)
	endif
	call insert(self.diagnostics, output, len(self.diagnostics))
endfunction
"# [ }}} ]
"# METHOD .print [ {{{ ]
function! vimtap#BatchHarness.print(...) dict
	" Do nothing.
endfunction
"# [ }}} ]
"# METHOD .flush [ {{{ ]
function! vimtap#BatchHarness.flush(...) dict
	" Do nothing.
endfunction
"# [ }}} ]
"## [ }}} ]
"### [ }}} ]
"##### [ }}} ]

"##### FUNCTIONS [ {{{ ]
"### FUNCTION vimtap#Quote [ {{{ ]
" Description:
" Quote the given argument and convert it to a String.
"
" Source:
function! vimtap#Quote(expr)
	if type(a:expr) == type("")
		return strtrans(a:expr)
	else
		return string(a:expr)
	endif
endfunction
"### [ }}} ]
"### FUNCTION vimtap#SetOutputFile [ {{{ ]
" Description:
" Create a new harness which writes output and diagnostics to the given file.
"
" Source:
function! vimtap#SetOutputFile(fname)
	let g:vimtap#TheHarness = g:vimtap#StandardHarness.New(a:fname)
endfunction
"### [ }}} ]
"### FUNCTION vimtap#FlushOutput [ {{{ ]
" Description:
" Flush the output of the current harness to the output sink.
"
" Source:
function! vimtap#FlushOutput()
	call g:vimtap#TheHarness.flush()
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Plan [ {{{ ]
" Description:
" Write the test plan to the output buffer.
"
" Example:
"   call vimtap#Plan(10)
"
" Source:
function! vimtap#Plan(tests)
	call g:vimtap#TheHarness.plan(a:tests)
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Ok [ {{{ ]
" Description:
" Ok is the simplest test function. The first argument is the result of an
" arbitrary test. In case the test succeeded, an ok line is printed into the
" test buffer. Otherwise a not ok line is printed. The description is appended
" to the test line.
"
" Example:
"   call vimtap#Ok(x == y, "x is equal to y")
"   call vimtap#Ok(IsFoo(x), "x is Foo")
"
" Source:
function! vimtap#Ok(test_result, description)
	call g:vimtap#TheHarness.ok(a:test_result, a:description)
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Is [ {{{ ]
" Description:
" Is is a bit more complicated than Ok. It takes two entities and compares
" them using ==. Some diagnostic output gives more information about, why the
" test failed than it is possible for Ok.
"
" Example:
"   call vimtap#Is(x, y, "x is equal to y")
"
" Source:
function! vimtap#Is(got, exp, description)
	let test_result = a:got == a:exp

	call vimtap#Ok(test_result, a:description)
	if !test_result
		call vimtap#Diag("Test '%s' failed:\n"
					\ . "expected: '%s'\n"
					\ . "but got:  '%s'",
					\ a:description, vimtap#Quote(a:exp), vimtap#Quote(a:got))
	endif
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Isnt [ {{{ ]
" Description:
" Isnt is similar to Is, but the generated value should be different from the
" supplied one.
"
" Example:
"   call vimtap#Isnt(x, y, "x is not equal to y")
"
" Source:
function! vimtap#Isnt(got, unexp, description)
	let test_result = a:got != a:unexp

	call vimtap#Ok(test_result, a:description)
	if !test_result
		call vimtap#Diag("Test '%s' failed:\n"
					\ . "got unexpected: '%s'",
					\ a:description, vimtap#Quote(a:got))
	endif
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Like [ {{{ ]
" Description:
" Like is similar to Is, but the it uses a regular expression which is matched
" against the passed in value. If the value matches the regular expression,
" then the test succeeds.
"
" Example:
"   call vimtap#Like(x, '\d\d', "x has two-digit number")
"
" Source:
function! vimtap#Like(got, re, description)
	let test_result = a:got =~ a:re

	call vimtap#Ok(test_result, a:description)
	if !test_result
		call vimtap#Diag("Test '%s' failed:\n"
					\ . "got: '%s'\n"
					\ . "does not match: /%s/",
					\ a:description, vimtap#Quote(a:got), a:re)
	endif
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Unlike [ {{{ ]
" Description:
" Unlike is similar to Like, but the regular expression must not match.
"
" Example:
"   call vimtap#Unlike(x, '^\s*$', "x contains non-whitespace")
"
" Source:
function! vimtap#Unlike(got, re, description)
	let test_result = a:got !~ a:re

	call vimtap#Ok(test_result, a:description)
	if !test_result
		call vimtap#Diag("Test '%s' failed:\n"
					\ . "got: '%s'\n"
					\ . "does match: /%s/",
					\ a:description, vimtap#Quote(a:got), a:re)
	endif
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Pass [ {{{ ]
" Description:
" Sometimes, in a more complicated setup, reaching a certain point in the
" control flow, means that we actually passed the test, even without
" checking some condition. So this is a simlpe convenience function for this.
"
" Example:
"   if (SomeCondition())
"       call vimtap#Ok(SomeSpecialCase(), "check some special case")
"   else
"       call vimtap#Pass("we are clear here")
"   endif
"
" Source:
function! vimtap#Pass(description)
	call vimtap#Ok(1, a:description)
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Fail [ {{{ ]
" Description:
" Similar to Pass, but fail the test.
"
" Example:
"   if (SomeCondition())
"       call vimtap#Ok(SomeSpecialCase(), "check some special case")
"   else
"       call vimtap#Fail("already lost")
"   endif
"
" Source:
function! vimtap#Fail(description)
	call vimtap#Ok(0, a:description)
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Diag [ {{{ ]
" Description:
" Print the given string into the output. Preface each line with a '#'.
"
" Example:
"   call vimtap#Diag("Some Diagnostic Message")
"
" Source:
function! vimtap#Diag(...)
	call call(g:vimtap#TheHarness.diag, a:000, g:vimtap#TheHarness)
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Skip [ {{{ ]
" Description:
" Sometimes it is useful to skip a certain number of tests, eg. when you
" the tests are related to some optional feature, which depends on some
" external library or functionality which might not be available.
"
" Example:
"   if (!vimtap#Skip(1, has("gui_running"), "Need GUI for this"))
"           vimtap#Ok(DoSomethingWhichNeedsGUI(), "Some GUI test")
"   endif
"
" Depending on whether the GUI is available or not, one gets either
"
"   ok 1 - Some GUI test
"
" (of course only if the test succeeded...), or
"
"   ok 1 # SKIP Need GUI for this
"
" Source:
function! vimtap#Skip(num_tests, guard, reason)
	return g:vimtap#TheHarness.skip(a:num_tests, a:guard, a:reason)
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Todo [ {{{ ]
" Description:
" Mark the following tests as todo. Failing todo tests are ignored. The
" standard Perl harnesses special mark succeeding todo tests.
"
" Example:
"   call vimtap#Todo(1)
"   call vimtap#Ok(DoSomethingThatStillFails(), "this test is ignored")
"
" Source:
function! vimtap#Todo(todo_tests)
	call g:vimtap#TheHarness.todo(a:todo_tests)
endfunction
"### [ }}} ]
"### FUNCTION vimtap#BailOut [ {{{ ]
" Description:
" Sometimes something really terrible happens, so that the testing must
" be stopped immediately. Then the test prints a "Bail out!" message
" and stops.
"
" Example:
"   if (!RunningInVim())
"       call vimtap#BailOut("Running under emacs! Arghh!")
"   endif
"
" Note:
" This function raises a "VimTAP:BailOut:" exception! Don't catch these
" or all curses of ye olden dayes shall be cast upon thee!
"
" Source:
function! vimtap#BailOut(reason)
	call g:vimtap#TheHarness.bail_out(a:reason)
endfunction
"### [ }}} ]
"##### [ }}} ]

"##### EPILOG [ {{{ ]
let &cpo = s:saved_cpo
unlet s:saved_cpo
"##### [ }}} ]
