# A tiny EDSL to write type-level-unit tests.

They can be embedded into the code, or existing unit tests.

The key feature is that the compiler checks the assertions and expectations made
in a 'TypeSpec' and right away rejects invalid types.

After all, with 'TypeError' GHC /is/ the test-runner.

If you accept to defer type checking and have invalid specs checked during test
execution, use
(should-not-typecheck)[https://github.com/CRogers/should-not-typecheck].
