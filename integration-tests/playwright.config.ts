// playwright.config.ts
import { PlaywrightTestConfig, devices } from '@playwright/test';

const config: PlaywrightTestConfig = {
  testDir: '.',
  testMatch: 'tests-playwright.ts',
  use: {
    headless: true,
    screenshot: 'off',
    video: 'off',
    // CLEANUP: probably don't need this
    viewport: { width: 1600, height: 1200 },
  },
};
export default config;
