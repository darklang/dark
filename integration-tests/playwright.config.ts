// Config file for integration tests
import { PlaywrightTestConfig, devices } from "@playwright/test";

const config: PlaywrightTestConfig = {
  testDir: ".",
  testMatch: "test-playwright.*",
  expect: {
    // timeout: 5000,
  },
  timeout: 15000,
  use: {
    // actionTimeout: 1000,
    headless: true,
    screenshot: "off",
    video: "on",
  },
};

export default config;
