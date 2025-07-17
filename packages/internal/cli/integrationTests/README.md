# Darklang CLI Integration Tests

## Running Tests

It is recommended to run the integration tests inside the dev container (required on Windows, recommended on Unix systems).
To run the tests, execute the following command in the terminal from the root project:

```bash
./scripts/run-cli run @Darklang.Internal.Cli.IntegrationTests.runIntegrationTests
```

## Test Types

**Regular CLI Tests**: Test individual CLI commands and check for exact output match against saved files

- Example: `darklang help`, `darklang version`

**Interactive Tests**: Test CLI interactive mode using `expect` automation

- Example: Starting CLI, typing commands, checking responses

## Key Files

- `integrationTests.dark` - Test definitions
- `test-framework.dark` - Test execution logic
- `run-integration-tests.dark` - Main test runner
- `test-config.dark` - Defines paths, VHS settings, timeout values, etc.
- `test-utils.dark` - Utility functions

## Test Results

- **Expected outputs**: `testResults/expected/`
- **Actual outputs**: `testResults/actual/` (temporary)
- **Failure recordings**: `testResults/failed-tests-gifs/` (GIF recordings of failed test runs)

## Adding New Tests

### Regular CLI Test

```fsharp
TestFramework.makeCliTest
  "Test Name"
  ["command", "args"]
  "expected-output-file.txt"
```

### Interactive Test

```fsharp
TestFramework.makeInteractiveCliTest
  "Test Name"
  [ TestFramework.InteractiveStep.SendInput "command"
    TestFramework.InteractiveStep.Wait <milliseconds>L
    TestFramework.InteractiveStep.ExpectOutput "expected response"
    TestFramework.InteractiveStep.SendQuit
  ]
  TestConfig.defaultInteractiveTimeout
```

## Features

- **Test setup**: First run creates expected output files
- **Diff output**: Shows exactly what differs when tests fail
- **GIF recordings**: Visualize interactive test failures using VHS
