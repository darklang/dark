Multifactor Auth
================

What's done:
============
- if someone sends you an email inviting you to enroll in MFA, you can
enroll
- if you have enrolled, you will be prompted on every login for MFA
- if you have not enrolled, you will not be prompted

What's not done:
================
- no way to enroll people in MFA without a manual step by us
- we could automate this - there is an API, we could add a button to our
UI that says "yes please turn on MFA for me"

## How to automate enrollment
I believe this can be done using `/api/v2/guardian/enrollments/ticket`; some
testing required. It's documented at
https://auth0.com/docs/api/management/v2/#!/Guardian/post_ticket; the samples
show the body requiring a `user_id` value, which we can get from the management
api (it's `auth0|some_string`, not the username or email address), and I think
we also want to set `send_mail` to true. The ops-login canvas is probably a good
place to start looking - it already does some auth against Auth0 in `/callback`, though I'm not
sure whether t he particular `grant_type: authorization_code` is the right one
for the management API.

Anyway, that'd get us the ability to hit the API from Dark; we'd then want to
expose an API of our own that accepts requests from a user and hook it up to a
button on a user's canvas, probably in the my-account menu somewhere.

How to turn on/off MFA for a given user
=======================================
## To enroll a user in MFA

Go to the users page https://manage.auth0.com/dashboard/us/darklang/users and
click to the user's details. You'll see a header "Multi-Factor Authentication".
In that will be the text "MFA is enabled for this user. Send an enrollment
invitation"; click the "Send an enrollment invitation" link; they'll get an
email telling them how to configure MFA. (NB: "enabled" means Auth0 allows it
for them, it does not mean they've added MFA to their account, or that we
require it.)

Once you've done that, I believe we will still not require MFA on login until
they successfully complete enrollment.

## To reset MFA for a user
If a user doesn't want MFA on anymore, or if they have lost access to their MFA
device, go to https://manage.auth0.com/dashboard/us/darklang/users , find the
user, and click the "Reset MFA" link on their page.


Types of MFA we support and why
===============================
Auth0 supports 5 types: Email OTP, Push to the Auth0 Guardian smartphone app,
SMS, Duo, and OTP apps (Google Authenticator, Authy, Auth0 Guardian, Microsoft
Authenticator, probably 1Password, etc. [Auth0 Guardian in this scenario does
not do push auth, you have to open it and get a 6-digit OTP; AIUI, it's push
auth that requires the enterprise plan.]).

The first 3 are only available to Enterprise plans, so we can't use them. Duo
can only be used if it is the _only_ MFA type provided, which we don't want. So
we are left with OTP apps, which is probably what most people use anyway.

Implementation details
======================
We do this using an Auth0 Rule; this rule can be seen at https://manage.auth0.com/dashboard/us/darklang/rules, and I include
the javascript below for future reference/backup purposes:

```javascript
function multifactorAuthentication(user, context, callback) {
  var ManagementClient = require("auth0@2.9.1").ManagementClient;
  var management = new ManagementClient({
    token: auth0.accessToken,
    domain: auth0.domain,
  });
  management.users.getGuardianEnrollments({ id: user.user_id }, function (
    err,
    enrollments
  ) {
    // debug logs
    /*
    console.log("ENROLLMENTS");
    console.log(enrollments);
    console.log("CONTEXT");
    console.log(context);
    */

    // we only require MFA on login if you have at least one enrolled MFA
    // authenticator
    var requireMFA = enrollments !== undefined && enrollments.length > 0;
    console.log("RequireMFA? " + requireMFA);
    if (requireMFA) {
      context.multifactor = {
        provider: "authenticator",

        // optional, defaults to true. Set to false to force authentication every time.
        // See https://auth0.com/docs/multifactor-authentication/custom#change-the-frequency-of-authentication-requests for details
        allowRememberBrowser: false,
      };
    }

    callback(null, user, context);
  });
}
```

We have Real-time Webtask Logs on, and that's where console logs from the above
rule will show up. I'm not sure how to deep-link to it, but you can get there
by going to https://manage.auth0.com/dashboard/us/darklang/extensions , then
filtering for Real-time Webtask Logs and clicking on the extension (it'll be the
only one that shows up). It's basically a browser-based `tail` of logs.
