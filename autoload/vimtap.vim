"##### HEADER [ {{{ ]
" Plugin:       VimTAP
" Version:      0.4.0-SNAPSHOT
" Author:       Meikel Brandmeyer <mb@kotka.de>
" Created:      Sat Apr 12 20:53:41 2008
"
" License:
" Copyright (c) 2008-2012 Meikel Brandmeyer, Frankfurt am Main
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
		let desc = "# TODO"
		if a:description != ""
			let desc .= " " . a:description
		endif
		let self.todos = self.todos - 1
	else
		if a:description != ""
			let desc = printf("- %s", a:description)
		else
			let desc = ""
		endif
	endif

	let result .= " " . self.test
	if desc != ""
		let result .= " " . desc
	endif
	call self.print(result)

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
		return "\"" . escape(strtrans(a:expr), '\"') . "\""
	else
		return string(a:expr)
	endif
endfunction
"### [ }}} ]
"### FUNCTION vimtap#ParseExpression [ {{{ ]
" Description:
" Parse the argument of a command as per <q-args>. An expression is either
" delimited by whitespace or by parens. Leading whitespace is removed.
"
" Returns:
" A list of the expression and the remaining parts of the input.
"
" TODO:
" Get a real parser. Delimiters eg. in strings can be tricky.
"
" Source:
function! vimtap#ParseExpression(input)
	let input = substitute(a:input, '^\s\+', '', '')
	if input[0] == '('
		return matchlist(input, '^\((.\{-})\)\(.*\)$')[1:2]
	elseif input[0] == '['
		return matchlist(input, '^\(\[.\{-}\]\)\(.*\)$')[1:2]
	elseif input[0] == '{'
		return matchlist(input, '^\({.\{-}}\)\(.*\)$')[1:2]
	elseif input[0] == '"'
		return matchlist(input, '^\("\%(\\[\"]\|[^\"]\)\{-}"\)\(.*\)$')[1:2]
	elseif input[0] == "'"
		return matchlist(input, '^\(''\%(\\[\'']\|[^\'']\)\{-}''\)\(.*\)$')[1:2]
	else
		return matchlist(input, '^\(\S\+\)\(.*\)$')[1:2]
	endif
endfunction
"### [ }}} ]
"### FUNCTION vimtap#ParseDescription [ {{{ ]
" Description:
" Parse the argument of a command as per <q-args>. The description is
" simply the remaining part of the input without a any leading whitespace.
"
" Source:
function! vimtap#ParseDescription(input)
	let input = substitute(a:input, '^\s\+', '', '')
	if input[0] == '"' || input[0] == "'"
		return vimtap#ParseExpression(input)[0]
	endif
	return '""'
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
"   call vimtap#Ok(x == y, 'x == y', "x is equal to y")
"   call vimtap#Ok(IsFoo(x), 'IsFoo(x)', "x is Foo")
"
" Source:
function! vimtap#Ok(test_result, qtest, description)
	call g:vimtap#TheHarness.ok(a:test_result, a:description)
	if !a:test_result
		call vimtap#Diag("Test '%s' failed:\n"
					\ . "expected: %s\n"
					\ . "to be true, but got false.",
					\ a:description, a:qtest)
	endif
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Is [ {{{ ]
" Description:
" Is is a bit more complicated than Ok. It takes two entities and compares
" them using ==. Some diagnostic output gives more information about, why the
" test failed than it is possible for Ok.
"
" Example:
"   call vimtap#Is(x, y, qx, "x is equal to y")
"
" qx is the quoted expression which gives raise to x.
"
" Source:
function! vimtap#Is(got, exp, qgot, description)
	let test_result = a:got == a:exp

	call g:vimtap#TheHarness.ok(test_result, a:description)
	if !test_result
		call vimtap#Diag("Test '%s' failed:\n"
					\ . "expected: %s\n"
					\ . "to be:    %s\n"
					\ . "but got:  %s",
					\ a:description, a:qgot, vimtap#Quote(a:exp),
					\ vimtap#Quote(a:got))
	endif
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Isnt [ {{{ ]
" Description:
" Isnt is similar to Is, but the generated value should be different from the
" supplied one.
"
" Example:
"   call vimtap#Isnt(x, y, 'x', "x is not equal to y")
"
" Source:
function! vimtap#Isnt(got, unexp, qgot, description)
	let test_result = a:got != a:unexp

	call g:vimtap#TheHarness.ok(test_result, a:description)
	if !test_result
		call vimtap#Diag("Test '%s' failed:\n"
					\ . "expected: %s\n"
					\ . "to be different from: %s",
					\ a:description, a:qgot, vimtap#Quote(a:got))
	endif
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Like [ {{{ ]
" Description:
" Like is similar to Is, but it uses a regular expression which is matched
" against the passed in value. If the value matches the regular expression,
" then the test succeeds.
"
" Example:
"   call vimtap#Like(x, '\d\d', 'x', "x has two-digit number")
"
" Source:
function! vimtap#Like(got, re, qgot, description)
	let test_result = a:got =~ a:re

	call g:vimtap#TheHarness.ok(test_result, a:description)
	if !test_result
		call vimtap#Diag("Test '%s' failed:\n"
					\ . "expected:   %s\n"
					\ . "with value: %s\n"
					\ . "to match:   /%s/",
					\ a:description, a:qgot, vimtap#Quote(a:got), a:re)
	endif
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Unlike [ {{{ ]
" Description:
" Unlike is similar to Like, but the regular expression must not match.
"
" Example:
"   call vimtap#Unlike(x, '^\s*$', 'x', "x contains non-whitespace")
"
" Source:
function! vimtap#Unlike(got, re, qgot, description)
	let test_result = a:got !~ a:re

	call g:vimtap#TheHarness.ok(test_result, a:description)
	if !test_result
		call vimtap#Diag("Test '%s' failed:\n"
					\ . "expected:     %s\n"
					\ . "with value:   %s\n"
					\ . "to not match: /%s/",
					\ a:description, a:qgot, vimtap#Quote(a:got), a:re)
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
	call g:vimtap#TheHarness.ok(1, a:description)
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
	call g:vimtap#TheHarness.ok(0, a:description)
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

"##### COMMANDS [ {{{ ]
"### COMMAND VimTapPrepareLocals [ {{{ ]
" Description:
" This is a private command used to remove locals used by Vimtap from the
" calling context.
"
" DON'T USE FROM CLIENT CODE!
"
" Source:
command! -nargs=* -bar VimTapPrepareLocals
			\ for local in [ <f-args> ] |
			\     if exists(local) |
			\         execute "unlet" local |
			\     endif |
            \ endfor
"### [ }}} ]
"### COMMAND Ok [ {{{ ]
" Description:
" Do a vimtap#Ok assertion. Executes the given expression and checks for
" its truthiness. Expressions containing whitespace must be wrapped in
" parentheses.
"
" The description is optional.
"
" Example:
"   Ok isFoo(x) "x is a Foo"
"   Ok (bar(x, y)) "bar of x and y is true"
"
" Source:
command! -nargs=1 Ok
			\ VimTapPrepareLocals vimtapMore vimtapExpr vimtapDesc |
			\ let [ vimtapExpr, vimtapMore ] = vimtap#ParseExpression(<q-args>) |
			\ let vimtapDesc = vimtap#ParseDescription(vimtapMore) |
			\ execute "call vimtap#Ok("
			\ . vimtapExpr . ", "
			\ . "\"" . escape(vimtapExpr, '\"') . "\", "
			\ . vimtapDesc . ")" |
			\ unlet vimtapMore vimtapExpr vimtapDesc
"### [ }}} ]
"### COMMAND Plan [ {{{ ]
" Description:
" Declare the plan of the test script.
"
" Example:
"   Plan 4
"
" Source:
command! -nargs=1 Plan call vimtap#Plan(<args>)
"### [ }}} ]
"### COMMAND Is [ {{{ ]
" Description:
" Do a vimtap#Is assertion. Compares the result of the expressions expected
" and actual. The expressions are arbitrary vimscript expressions. Expressions
" containing whitespace have to put into parentheses.
"
" The description is optional.
"
" Example:
"   Is (x + 1) 5 "x is 4"
"
" Source:
command! -nargs=1 Is
			\ VimTapPrepareLocals vimtapMore vimtapActual vimtapExpected vimtapDesc |
			\ let [ vimtapActual, vimtapMore ] = vimtap#ParseExpression(<q-args>) |
			\ let [ vimtapExpected, vimtapMore ] = vimtap#ParseExpression(vimtapMore) |
			\ let vimtapDesc = vimtap#ParseDescription(vimtapMore) |
			\ execute "call vimtap#Is(" . vimtapActual . ", "
			\ . vimtapExpected . ", "
			\ . "\"" . escape(vimtapActual, '\"') . "\", "
			\ . vimtapDesc . ")" |
			\ unlet vimtapMore vimtapActual vimtapExpected vimtapDesc
"### [ }}} ]
"### COMMAND Isnt [ {{{ ]
" Description:
" Do a vimtap#Isnt assertion. Compares the result of the expressions unexpected
" and actual. The expressions are arbitrary vimscript expressions. Expressions
" containing whitespace have to put into parentheses.
"
" The description is optional.
"
" Example:
"   Isnt (x + 1) 5 "x is not 4"
"
" Source:
command! -nargs=1 Isnt
			\ VimTapPrepareLocals vimtapMore vimtapActual vimtapUnexpected vimtapDesc |
			\ let [ vimtapActual, vimtapMore ] = vimtap#ParseExpression(<q-args>) |
			\ let [ vimtapUnexpected, vimtapMore ] = vimtap#ParseExpression(vimtapMore) |
			\ let vimtapDesc = vimtap#ParseDescription(vimtapMore) |
			\ execute "call vimtap#Isnt("
			\ . vimtapActual . ", "
			\ . vimtapUnexpected . ", "
			\ . "\"" . escape(vimtapActual, '\"') . "\", "
			\ . vimtapDesc . ")" |
			\ unlet vimtapMore vimtapActual vimtapUnexpected vimtapDesc
"### [ }}} ]
"### COMMAND Like [ {{{ ]
" Description:
" Do a vimtap#Like assertion. Tries to match the expected expression with the
" given regular expression. The expressions are arbitrary vimscript expressions.
" Expressions containing whitespace have to put into parentheses.
"
" The description is optional.
"
" Example:
"   Like ("1" . "2") '\d\d' "has two digits"
"
" Source:
command! -nargs=1 Like
			\ VimTapPrepareLocals vimtapMore vimtapActual vimtapExpr vimtapDesc |
			\ let [ vimtapActual, vimtapMore ] = vimtap#ParseExpression(<q-args>) |
			\ let [ vimtapExpr, vimtapMore ] = vimtap#ParseExpression(vimtapMore) |
			\ let vimtapDesc = vimtap#ParseDescription(vimtapMore) |
			\ execute "call vimtap#Like("
			\ . vimtapActual . ", "
			\ . vimtapExpr . ", "
			\ . "\"" . escape(vimtapActual, '\"') . "\", "
			\ . vimtapDesc . ")" |
			\ unlet vimtapMore vimtapActual vimtapExpr vimtapDesc
"### [ }}} ]
"### COMMAND Unlike [ {{{ ]
" Description:
" Do a vimtap#Unlike assertion. Tries to match the expected expression with
" the given regular expression and fails in case it actually matches. The
" expressions are arbitrary vimscript expressions. Expressions containing
" whitespace have to put into parentheses.
"
" The description is optional.
"
" Example:
"   Unike ("A" . "B") '\d\d' "has not two digits"
"
" Source:
command! -nargs=1 Unlike
			\ VimTapPrepareLocals vimtapMore vimtapActual vimtapExpr vimtapDesc |
			\ let [ vimtapActual, vimtapMore ] = vimtap#ParseExpression(<q-args>) |
			\ let [ vimtapExpr, vimtapMore ] = vimtap#ParseExpression(vimtapMore) |
			\ let vimtapDesc = vimtap#ParseDescription(vimtapMore) |
			\ execute "call vimtap#Unlike("
			\ . vimtapActual . ", "
			\ . vimtapExpr . ", "
			\ . "\"" . escape(vimtapActual, '\"') . "\", "
			\ . vimtapDesc . ")" |
			\ unlet vimtapMore vimtapActual vimtapExpr vimtapDesc
"### [ }}} ]
"### COMMAND Pass [ {{{ ]
" Description:
" Declare an always-pass assertion with the given description.
"
" Example:
"   Pass "always pass"
"
" Source:
command! -nargs=1 Pass call vimtap#Pass(<args>)
"### [ }}} ]
"### COMMAND Fail [ {{{ ]
" Description:
" Declare an always-fail assertion with the given description.
"
" Example:
"   Fail "always fail"
"
" Source:
command! -nargs=1 Fail call vimtap#Fail(<args>)
"### [ }}} ]
"### COMMAND Todo [ {{{ ]
" Description:
" Declare some of the following tests to be TODO.
"
" Example:
"   Todo 5
"
" Source:
command! -nargs=1 Todo call vimtap#Todo(<args>)
"### [ }}} ]
"### COMMAND BailOut [ {{{ ]
" Description:
" Bail out of the test script with the given message.
"
" Example:
"   if (!RunningInVim())
"       BailOut "Running under emacs! Arghh!"
"   endif
"
" Note:
" This function raises a "VimTAP:BailOut:" exception! Don't catch these
" or all curses of ye olden dayes shall be cast upon thee!
"
" Source:
command! -nargs=1 BailOut call vimtap#BailOut(<args>)
"### [ }}} ]
"##### [ }}} ]

"##### EPILOG [ {{{ ]
let &cpo = s:saved_cpo
unlet s:saved_cpo
"##### [ }}} ]
