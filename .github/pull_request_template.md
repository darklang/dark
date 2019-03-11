- [ ] Include [Trello](https://trello.com/b/B25On0K9/feb-2019)  link
- [ ] Describe the goals, problem and solution ([PR guide](https://docs.google.com/document/d/1IeQdEh7ROhNV6Z2mJdu35E6r8XtFOkOhyhIeAvm8amE/edit))
- [ ] Make sure info from this description is also in comments
- [ ] Include before/after screenshots/gif if applicable
- [ ] Add intended followups as trellos 
- [ ] If risky, discuss your reversion strategy
- [ ] If this is fixing a regression, add a test

Reviewer checklist:
- Product:
  - [ ] Does this match the goal of the PR or trello?
  - [ ] Does this add or change product features not discussed in the goals?
- User facing:
  - [ ] Could this cause a silent change in behaviour of user programs, eg an output format or function behaviour?
  - [ ] Is there consistent naming of new user concepts?
- Engineering: 
  - [ ] If this was a regression, is there a test?
  - [ ] Would comments help future understanding somewhere?
  - [ ] Double check any change related to the serialization format.

