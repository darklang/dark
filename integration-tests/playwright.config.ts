// Config file for integration tests
import { PlaywrightTestConfig, devices } from '@playwright/test';

const config : PlaywrightTestConfig = {
  testDir: ".",
  testMatch: "test-playwright.*",
  use: {
    // actionTimeout: 2000,
    headless: true,
    screenshot: "off",
    video: "on"
  },
};

export default config;
