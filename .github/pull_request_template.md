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
  - [ ] Existing stdlib and language semantics are unchanged.
  - [ ] Existing granduser HTTP responses are unchanged
  - [ ] All existing canvases should continue to work
  - [ ] New features are documented in the User Manual or Trello filed
- Engineering:
  - [ ] Tests are included or unnecessary (required for regressions)
  - [ ] Functions and variables are well-named and self-documenting.
  - [ ] Comments have been added for all explanations in PR review comment.
  - [ ] Serialization format changes look good and have been double-checked and tested against local prodclone.
  - [ ] Unneeded code has been removed.

