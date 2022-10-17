// Config file for integration tests
import { PlaywrightTestConfig } from "@playwright/test";

const config: PlaywrightTestConfig = {
  testDir: ".",
  testMatch: "tests.ts",
  expect: {
    timeout: 5000,
  },
  // in ms. 30000 = 30s
  timeout: 30000,
  use: {
    actionTimeout: 5000,
    headless: true,
    trace: "retain-on-failure",
    screenshot: "off",
    video: "on",
  },
  reporter: [
    ["list"],
    ["json", { outputFile: "rundir/test_results/integration_tests.json" }],
    ["junit", { outputFile: "rundir/test_results/integration_tests.xml" }],
  ],
};

export default config;
