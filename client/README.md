# Editor

The Dark Editor is the client-side app where you write and edit code.


## Directory structure

### client/src

#### app
Generic code that we'd need for any app, including Types, Error handling, integration tests, feature flags, browser communication.

#### core
The parts of the app that cross everything: types, decoders, defaults.

#### prelude
To be imported into every file, it includes types, tablecloth, and also
patches around 3rd party libraries we use.

#### canvas
Code relating to the overall canvas, it's scaffold and it's navigation: the sidebar, the viewport, collaboration (for now).

##### toplevels
Code relating to toplevels (UserFunctions, UserTypes, Handlers and Databases). including views and introspection.

#### rpc
How we talk to the server.

#### analysis
Used as part of the analysis engine.

#### forms
The form editor is used to edit blankOrs.

#### fluid
The fluid editor is used to edit ASTs. Includes most things to do with code
editing, such as refactoring.

### client/workers
Background workers for performance/latency.

### client/lib
For compiler intermediate files. You can safely ignore it.

### client/test
Tests, along with a fuzzer and testframework. See docs/unittests.md and
docs/fuzzer.md.
