[![Build Status](https://travis-ci.org/Gizra/elm-debouncer.svg?branch=master)](https://travis-ci.org/Gizra/elm-debouncer)

# elm-debouncer

Yet another debounce module!

Of the many debounce modules available in the Elm package database, the best
ones appear to be:

- [jinjor/elm-debouce](http://package.elm-lang.org/packages/jinjor/elm-debounce/latest)
- [mpizenberg/elm-debounce](http://package.elm-lang.org/packages/mpizenberg/elm-debounce/latest)

So, why another one? The main thing they are missing that I would want is a
convenient notion of "cancelation" -- that is, cases in which you want to
cancel a scheduled event. (You could achieve cancelation on top of one or the
other of those modules, but it would be awkward or unreliable).

There are also some Elm debouncers based on effects modules.

- [unbounce/elm-debounce](https://github.com/unbounce/elm-debounce)
- [mceldeen/elm-debouncer](https://github.com/mceldeen/elm-debouncer)

They have the advantage that they can manage state internally, rather than
forcing you to modify your code to handle state updates. However, this requires
that you supply a global "key" with your debounce requests, to distinguish one
debouncer from another. This is awkward -- it seems nicer to avoid the need for
globally distinct strings, by having the client code explicitly manage the
state.

### Design considerations

What is debouncing, in essence? Bascally, it is a way of consolidating
something over time, and then dealing with those things as a group for that
time period. So, it is a kind of `inGroupsOf`, except that the groups are
defined by a time interval, rather than the element indexes.

This grouping by time implies several questions:

- What's the time interval?

  Here the design question is whether this ought to be the same for each thing
  that gets handled by a debouncer, or whether it could vary from one thing to
  the next? To my mind, if you think of debouncing as a kind of "inGroupsOf
  over time", then the most sensible thing is for the grouping to be a property
  of the debouncer itself, not each thing that is handled. (To group a list,
  for instance, you wouldn't change the criteria for each item in the list).
  However, there is no reason that the grouping time interval couldn't itself
  change over time. So, it ought to be a property of the debouncer, but there
  should be a capacity to change it. (This hasn't been impelemented yet, but
  could be).

- Is the first thing special?

  Sometimes, you want to group things over time, except you want to emit the first
  thing right away. Or, to generalize, perhaps you want a different time interval
  when the debouncer newly becomes "unsettled" (perhaps a time interval of 0) than
  when it is already collecting input.

- Debouncing vs. throttling.

  The question here is whether we wait for the inputs to "settle" before emitting
  anyhing, or whether we emit on an interval while the debouncer is "in progress."

  Combining the last two questions, there seem to be three intervals that are
  relevant:

  - When "settled" and we receive a new input, what time interval do we wait before
    first emitting something (possibly `Just 0`, if we want to emit the input
    immediately when becoming unsettled, and possibly `Nothing` if we don't
    want to treat the first input specially).

  - When "in progress", how long do we wait with no inputs until we should consider
    ourselves "settled" again?

  - When "in progress", what's the maximum time we should wait before emitting what
    we've got so far (possibly forever, if we don't want to emit anything until
    we're settled).

- How should you combine things?

  For things which happen within the relevant interval, how should they be
  combined?  The "naive" approach to this would be to provide just the last
  thing, or just the first thing, or just the first and last. But that's not
  how `inGroupsOf` would work -- it would give you all the things, and you could
  decide which you want.

  However, that would potentially hang onto a lot of interim data that you're
  just going to throw away. So, even better would be to let you provide your own
  "folding" function, to decide how the inputs get combined. (I got this idea
  from the Haskell [fold-debounce](https://hackage.haskell.org/package/fold-debounce)
  package).

- What are the things?

  So, what are these "things" we grouping over time? In Elm, the basic
  candidates are tasks or messages, since those are the things that happen at
  some particular time. In a way, you could implement one in terms of the
  other, since you can construct a Task to send a message, and you can use a
  message to perform a Task.

  One difficulty with working with tasks as the "things" is that once you've
  constructed a Task, you can't really know anything about it. So, in a way,
  it's more convenient to accumulate messages than tasks (since messages are
  just data).

  But really, it would be best for some layer of the debouncing to be
  indifferent as to what the "things" are. It is merely provided the "things"
  at various times, and emits them, grouped in some way, at other times.

- How does one "provide" and "emit" things?

  If one is not using an effects manager, then it seems easiest to provide things
  via messages (e.g. through the normal `update` function).

  As far as emitting things goes, there would be a number of options. You could
  imagine an extra parameter returned from an `update` function (meaning: I've just
  emitted something -- here it is -- do something with it). Or, you could imagine
  requiring the client of the package to provide a "mapping" function, so that the
  package can emit the client's own `Msg` type. The former seems more flexible, since
  it would not be difficult for the client to do its own mapping.

## API

For the detailed API, see the
[Elm package site](http://package.elm-lang.org/packages/Gizra/elm-debouncer/latest),
or the links to the right, if you're already there.

## Installation

Try `elm-package install Gizra/elm-debouncer`

## Development

Try something like:

    git clone https://github.com/Gizra/elm-debouncer
    cd elm-debouncer
    npm install
    npm test

You can then find the compiled examples in the `build` folder.
