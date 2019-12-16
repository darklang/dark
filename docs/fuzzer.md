# Fuzzer / property-based testing

Dark has a fuzzer in client/__tests__/fluid_fuzzer.ml, which creates random programs for testing the editor.

The intent is to allow you to write property-based tests, and then find
violations of those tests. For example, if you were having problems with
deleting, you would create a test that checked you could delete the text,
and then throw thousands of random programs at it. This would allow you to
find every bug in deletion.

The fuzzer is not intended to run in CI (you do not want CI to fail just
because the fuzzer happened to find a new error). Instead, run the fuzzer
when you're working on a feature - any time you find a new failure, turn
that into a test.

## Is Fuzz-testing right for me?

This is applicable if:
- there are lots of edge cases and you struggle to find them all
- or you keep seeing bugs in one product area
- you can think of a unit test which would correctly test any input

Some examples:

- Test deletion by completely deleting the input: the success criteria will
  be blank ast with the cursor at the start.

- Test copy/paste by roundtripping: the input and output are expected to be
  identical.

- Test the left key: the position after pressing left should be less than
  the start position.


## How to use

Write a test that will work for any input, and add it to
client/__tests__/fuzz_tests.ml. See the existing examples there.

To test it, you can run Jest directly:

  ./node_modules/.bin/jest --bail 1 --verbose --testPathPattern fuzz_tests

Once you've fpund a failiure, add that test to the test suite to prevent
regression. 

## Other info

Tests are deterministic based on the in Fluid_fuzzer.ml. Set the seed in each test.

Fuzz testing is a work in progress, and you'll probably have to talk to Paul
to get it to work for you.

