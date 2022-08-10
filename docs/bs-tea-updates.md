# Components in the client

The client use The Elm Architecture. This a common pattern for building frontend
architectures, used in many languages and platforms.

## Basics

### Model

A TEA app uses a single global state object called a `model`. Anytime we want to
change program state, we update the model. There is no other program state: all view,
events, HTTP requests, etc, can only change the program state by updating the model.

### Event-loop

TEA uses an "event-loop" pattern:

- starting with the existing model
- process all events, updating the model
- create an in-memory DOM by running the `view` function over the model
- diff the in-memory DOM against the one from the last event loop
- update the DOM

### Messages

Events are sent as the Msg type. This is user defined, and might for example be
`ClickInviteSubmitBtn`. When writing view code, we specify which messages are created
for DOM events. We also specify what messages to receive for the callbacks of API
calls. Messages are also used to wrap events that we can subscribe to, such as
keystrokes, custom browser events, history chages, etc. We receive all inputs this
way.

### Commands

Commands are used to communicate with TEA, and generally encapsulate something
stateful or outside the model, such as API calls or sending events to the browser.
When a message is received, our `update` handler can return Commands that TEA will
execute in order at the end of the loop.

## Dark-specific additions

A downside of the Elm Architecture is the everything is in one big model. That means
that changes are nearly all done in Main.res, and we end up with thousands of lines
of update code. In particular, that update code is all in Main.res, and us far away
from the rest of the code for that conceptual model.

In addition, since we're in a functional language, we're not allowed cycles between
modules. That makes it hard to have a component that defines its types and its
functions and views.

Finally, sometimes many different parts of the system need to do the same work.

### Modifications

To solve these problems, we came up with `Modifications`. Before we explain
modifications, it's important the realize that they were a mistake. A bad one. One
that we are trying to move away from. Some initial discussion of the mistake is in
https://github.com/darklang/dark/pull/2037.

Modifications were initially an attempt to reuse update code when components did the
same thing. In retrospect, we'd have been better off using functions for this. So now
Modifications just add a confusing layer.

We've been slowly trying to address this, by adding the
`ReplaceAllModificationsWithThisOne` modification. The idea is that we should be able
to update a component with a simple function call along with the data from the model
and/or the message.

### Effects

One issue we've hit with that is that sometimes a component change needs to make a
"Cross-Component Call". An example of a CCC is updating the pop-up toast
notification.

Due to cycles in the dependency graph, we're often (usually!) not able to call
directly into other components. "Effect" give us an out: we return them from a
component's `update` function, and act on them immediately, hopefully just directly
calling functions in another component from `Main.res`.
