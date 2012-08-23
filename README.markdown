# VimTAP – a TAP implementation for Vim

## Description

The [Test Anything Protcol](http://testanything.org) is a text based protocol
wide spread in the perl world. It allows to decouple the test result generation
from the test result analysis. Since TAP is easy to implement one can easily
combined different languages in one test suite and have all test report their
results in TAP format. In this way a single analysis step can be use to handle
all the test results.

And still TAP can be read by a human being if necessary.

This project uses [tap4j](http://tap4j.sf.net) to integrate the run vim tests
into the [gradle](http://www.gradle.org) build system.

## Example

    Plan 4
    Ok (1 == 1) "One is equal to one"
    Is (1 + 1) 3 "One + one is three"
    Like "Hello, World!" ', ' "String contains a comma"
    Todo 1
    Ok world_domination "take over the world muharharhar"

This test script generate the following output:

    1..4
    ok 1 - One is equal to one
    not ok 2 - One + one is three
    # Test 'One + one is three' failed:
    # expected: (1 + 1)
    # to be:    3
    # but got:  2
    ok 3 - String contains a comma
    not ok 4 # TODO take over the world muharharhar

## License

VimTAP is distributed under the terms of the MIT license.
