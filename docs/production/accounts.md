# Accounts

List of accounts used by Dark as part of running Dark

# Developing

- [github](https://github.com/darklang)
  for source code control, issue-tracking, etc.
  - [dark](https://github.com/darklang/dark)
  - [docs](https://github.com/darklang/docs)
    hosted on github pages
  - [dockerfile](https://github.com/darklang/dockerfile)
  - [dark-cli](https://github.com/darklang/dark-cli)
- `ops` account on [darklang.com](https://darklang.com)
  we maintain an `ops` account shared by Dark employees.
  - `ops-adduser`
  - `ops-circleci`
  - `ops-corp-site`
  - `ops-login`
  - `ops-presense`
- [Slack (dark community)](https://darklang.com/slack-invite)

# Deploying

- [CircleCI](https://app.circleci.com/pipelines/github/darklang) for continuous integration
- [LaunchDarkly](https://launchdarkly.com) for feature flags

# Hosting

- [Google Cloud](http://cloud.google.com)
  - GKE
  - Cloud SQL
  - Load Balancers
  - CDN for static assets (both ours and users')
  - DNS
  - Google Domains
  - PubSub for queues/crons
  - support
- [auth0](https://manage.auth0.com/dashboard) user account management
- [pusher.com](https://dashboard.pusher.com/apps) supports "real-time" pushes of information to the Editor
- [algolia](https://www.algolia.com) search engine used to support [Darklang docs](https://docs.darklang.com)
- [CloudFlare](https://cloudflare.com) slight usage for some DNS
- [PositiveSSL](https://www.positivessl.com) manages certificates for darksa.com
- [let's encrypt](https://letsencrypt.org) certificate management and custom domains
  see [custom-domains](./custom-domains.md)
- [brex](https://www.brex.com) how we pay for everything
- [name.com](http://name.com) TODO what do we use this for?
- [Ghost.org](https://ghost.org) powers [the blog](https://blog.darklang.com)
- [ToDesktop](https://todesktop.com) manages the desktop client of Dark
- [ngrok](https://ngrok.com) useful for exposing localhost ports externally

# Monitoring

- [Honeycomb](https://honeycomb.io) for monitoring, production analysis, etc.
  see [honeycomb.md](./honeycomb.md)
- [Rollbar](https://rollbar.com) for recording, exploring, and resolving bugs
- [PagerDuty](https://pagerduty.com) for alerting when things are broken
  - triggers from rollbar, pingdom and honeycomb
- [Better UpTime](https://betteruptime.com) for alerting when things are broken (newer)
- [Pingdom](https://pingdom.com) TODO: where do we use this?
- [sslmate](https://sslmate.com) TODO: where do we use this?
- [Dr. Link Check](https://drlinkcheck.com) TODO: where do we use this?

# User communications, analytics

- [Mailchimp](https://mailchimp.com) TODO: where do we use this?
- [Mandrill](https://mandrillapp.com) automated emails
- [Heap Analytics](https://heap.io) for analytics, but not really used
- [FullStory](https://fullstory.com) for user insight, but not really used
