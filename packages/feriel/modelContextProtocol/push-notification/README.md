# Push Notification MCP Server

This is an MCP server that provides push notification capabilities through multiple services.

## Features

- **Pushover notifications**: Send notifications via the Pushover
- **ntfy.sh notifications**: Send notifications via the ntfy.sh

## Setup

### Adding to Claude Desktop

To add this MCP server run:

```bash
claude mcp add push-notifications ./scripts/run-cli run @Feriel.ModelContextProtocol.PushNotification.Server.main --env PUSHOVER_USER_KEY=your_pushover_user_key --env PUSHOVER_APP_TOKEN=your_pushover_app_token
```

### Environment Variables

For Pushover notifications, you'll need:

- `PUSHOVER_USER_KEY`: Your Pushover user key
- `PUSHOVER_APP_TOKEN`: Your Pushover application token

For ntfy.sh notifications, no authentication is required - just specify a topic name.
