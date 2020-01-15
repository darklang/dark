# Bucklescript-tea and key and uniq

When we started using bucklescript-tea, we misunderstood how to use `key`, `uniq` (and to a certain extent `Vdom.noNode` and `Vdom.noProp`). A lot of code uses them incorrectly; here's how to use them correctly.

## The problems

Bucklescript-tea does vdom diffing: when it calls `view`, it generates a
virtual dom, then it diffs the virtual dom to determine what changes it
needs to make to the real DOM as a result.

# Uniq

A recognized problem in Virtual DOMs (Elm and React have this too, for
example), is knowing when a node has been changed. A common place for this
is in lists: if you previously generated a list of virtual nodes, and then
change their order, how do I know that what I'm seeing is the old list in a
new order?

React and Elm have a concept called a "key", which solves this. Bs-tea also
has a key, but it calls it "uniq" instead. Confusingly, it has a similar but
different concept which is called a "key", which is really confusing.

Uniq is a property on a node to identify it. If you have a list of elements
with uniqs "a", "b", and "c", and you reorder the list, the "uniq" string
will tell bs-tea that they are the same node. That will cause bs-tea to continue it's diffing in the right place.

If it can't tell, it will just blow away the existing nodes and recreate
everything beneath it.


# Vdom.noNode

A common way for a list to be blown away is when toggling an optional
element in the list. For example, the documentation box only sometimes
shows. When it appears, bs-tea doesn't know that the list has only changed
slightly, and so it blows it away and starts again. This leads to
performance problems.

Using uniq is one way to fix this, but probably not the right way. `uniq` is
really designed for lists that change order. For something like this, the
solution is to always include a node instead - that way the list stays in
the same order and you don't need to include `uniq` tokens everywhere.

`Vdom.noNode` is an empty node designed for exactly this purpose. When the
optional node appears or disappears, bs-tea will have a node to swap in/out,
without changing anything else. This means that not only does it not do
excess work to create the node, it also avoids rerendering.

# Vdon.noProp

This is exactly like Vdom.noNode, except for node properties.

# Key

`key` is a performance optimization that means that bs-tea will never
recurse into this node if the key is that same as  in the previous vdom. If
the key is equal, it will not compare the actual nodes, their properties, or
children. It will simply move on.

Note that this does not replace our cache - the cache caches the generated
vdom, but does not prevent bs-tea from recursing into it.

To use `key` well, you need to make sure that the key depends on every value
within it. If not, the nodes will not update when any values that are missed
are updated. We have found that the fastest way to turn values into a key
are to use `Js.Json.stringifyAny`.


# Key vs uniq

Most places where we use `key` in the codebase were actually intended to be
`uniq`.

Given two nodes that bs-tea is comparing, if they have different `key`s or
different `uniq`s, bs-tea will know they are not the same in both cases. If
they have the same `uniq`, bs-tea will still recurse into it and compare
subnodes. If they have the same `key`, bs-tea will stop there.

## Closed over values

An important consideration in using keys is that sometimes the value of a property is a function. Since OCaml cannot compare functions, you need to use the `key` to tell bs-tea whether the nested value is the same or not. If you do not include all data in the key, there will be a bug.

If you use `uniq` instead, it will just recalculate it each time, which has
been known to cause events to be lost in testcafe tests.
