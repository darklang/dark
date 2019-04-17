- [ ] Include [Trello](https://trello.com/b/B25On0K9/feb-2019)  link
- [ ] Describe the goals, problem and solution ([PR guide](https://docs.google.com/document/d/1IeQdEh7ROhNV6Z2mJdu35E6r8XtFOkOhyhIeAvm8amE/edit))
- [ ] Make sure info from this description is also in comments
- [ ] Include before/after screenshots/gif if applicable
- [ ] Add intended followups as trellos
- [ ] If risky, discuss your reversion strategy
- [ ] If this is fixing a regression, add a test

Reviewer checklist:
- Product:
  - [ ] PR matches stated goal and Trello ticket
  - [ ] Out-of-scope product changes have been explained
  - [ ] I pulled the branch and tested out the feature.
- User facing:
  - [ ] No existing stdlib function has had its behaviour changed.
  - [ ] Existing granduser HTTP responses are unchanged
  - [ ] All existing canvases should continue to work
  - [ ] New features are documented in the User Manual or Trello filed
- Engineering:
  - [ ] Tests are included where appropriate (especially if this is a regression fix!)
  - [ ] Functions and variables are well-named and self-documenting.
  - [ ] Comments are present when appropriate and are understandable.
  - [ ] Serialization format changes look good and have been double-checked and tested against local prodclone.

