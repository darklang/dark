import { ConsoleMessage, TestInfo } from "@playwright/test";

// This modules exists to track console logs and errors against individual
// tests; we track these logs, and at the end of test execution:
// - we ensure there are no error logs, and mark the test as failed otherwise
// - we persist the logs to disk

interface MessagesHolder {
  messages: ConsoleMessage[];
}
export function initMessages(testInfo: TestInfo) {
  let ti = <MessagesHolder & TestInfo>testInfo;
  ti.messages = [];
}

export function getMessages(testInfo: TestInfo) {
  let ti = <MessagesHolder & TestInfo>testInfo;
  return ti.messages;
}

export function saveMessage(testInfo: TestInfo, msg: ConsoleMessage) {
  let ti = <MessagesHolder & TestInfo>testInfo;
  ti.messages.push(msg);
}

export function clearMessages(testInfo: TestInfo) {
  let ti = <MessagesHolder & TestInfo>testInfo;
  ti.messages = [];
}
