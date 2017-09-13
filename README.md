[![Build Status](https://travis-ci.org/Gizra/elm-debouncer.svg?branch=master)](https://travis-ci.org/Gizra/elm-debouncer)

# elm-debouncer

"Debouncing" is the process of taking a stream of inputs, over time, and
emitting some kind of output, at certain intervals. So, it lets you "smooth
out" events, so you have some control over how soon (or how fast) they are
processed.

One classic use-case is auto-complete while typing. As each character is typed,
you could send a request to a server asking for possible completions. But you
don't want to slow things down while a person is typing quickly. So, you can
debounce the server request, so that it will only actually take place once
nothing has been typed for a few moments. By doing so, you've taken a stream of
inputs over time (the characters being typed), and you've emitted an output at
an appropriate moment, defined by some kind of interval.

This is, of course, a familiar concept, and there are many debounce modules
available for Elm already. My favourites are:

- [jinjor/elm-debounce](http://package.elm-lang.org/packages/jinjor/elm-debounce/latest)
- [mpizenberg/elm-debounce](http://package.elm-lang.org/packages/mpizenberg/elm-debounce/latest)

So, why another one?

One practical concern was that I needed an easy way to "cancel" a pending event.
(You could implement cancelation on top of other debouncers, but it appeared that
it would not be entirely straightforward). It also seemed that it would be possible
to consolidate different approaches to debouncing in an interesting way.

If that's enough introduction for you, do feel free to skip to the detailed
API, at the
[Elm package site](http://package.elm-lang.org/packages/Gizra/elm-debouncer/latest),
or the links to the right, if you're already there. If, on the other hand, you'd
like some more philosophizing about debouncers, read on!

### Grouping by time

Essentially, debouncing is the processing of organizing things in groups
over time. You might analogize it to the `groupsOf` function from
[elm-community/list-extra](http://package.elm-lang.org/packages/elm-community/list-extra/latest)

    groupsOf : Int -> List a -> List (List a)

Consider what `groupsOf` does. It takes a list, and hands you back the same
things, just organized differently. Debouncing is conceptually similar.
It takes some inputs, and (eventually) hands them back to you, just organized
differently. Now, with debouncing, this doesn't happen all at once -- you
don't have the whole list at once. Instead, it happens over time. So, you
might say that debouncing is the process of grouping events according to time.

The idea of grouping events by time implies several questions:

- What's the time interval?

  If we're grouping things over time, then the first question you might ask is:
  over what period of time? This would be like the first parameter to
  `groupsOf` -- that is, in groups of how many? Of course, it's a little
  different, in that you aren't counting the number of things, but instead the
  time period over which they happen.

- Is the first thing special?

  Sometimes, you want to group things over time, except you want to emit the
  first thing right away. Or, to generalize, perhaps you want a different time
  interval when the debouncer newly becomes "unsettled" (perhaps a time
  interval of 0) than when it is already collecting input.

- Debouncing vs. throttling.

  Do we want to wait for the inputs to "settle" before emitting anything, or
  should we emit on an interval while the debouncer is "in progress."

Considering these three questions together, there seem to be three intervals
that are relevant:

- When "settled" and we receive a new input, what time interval do we wait before
  first emitting something (possibly `Just 0`, if we want to emit the input
  immediately when becoming unsettled, and possibly `Nothing` if we don't
  want to treat the first input specially).

- When "in progress", how long do we wait with no inputs until we should consider
  ourselves "settled" again?

- When "in progress", what's the maximum time we should wait before emitting what
  we've got so far (possibly forever, if we don't want to emit anything until
  we're settled).

#### How do we combine things?

For things which happen within the relevant interval, how should they be
combined?  The "naive" approach to this would be to provide just the last
thing, or just the first thing, or just the first and last. But that's not how
`groupsOf` would work for lists -- it would give you all the things, and you
could decide which you want.

Now, that would potentially hang onto a lot of interim data that you're just
going to throw away. So, even better would be to let you provide your own
"folding" function, to decide how the inputs get combined. (I got this idea
from the Haskell
[fold-debounce](https://hackage.haskell.org/package/fold-debounce)
package).

#### What are the things?

So, what are these "things" we grouping over time? In Elm, the basic candidates
are tasks or messages, since those are the things that happen at some
particular time. In a way, you could implement one in terms of the other, since
you can construct a `Task` to send a message, and you can use a message to
perform a `Task`.

One difficulty with working with tasks as the "things" is that once you've
constructed a `Task`, you can't really know anything about it. So, in a way,
it's more convenient to accumulate messages than tasks (since messages are just
data).

But really, it would be best for some layer of the debouncing to be indifferent
as to what the "things" are. It is merely provided the "things" at various
times, and emits them, grouped in some way, at other times, whatever they are.

#### How does one "provide" and "emit" things?

It will be apparent that a debouncer needs to keep some state, and schedule some
events. There are some Elm debouncers that use effects modules to do this.

- [unbounce/elm-debounce](https://github.com/unbounce/elm-debounce)
- [mceldeen/elm-debouncer](https://github.com/mceldeen/elm-debouncer)

This is convenient because the debouncer can manage its own state -- you don't
have to explicitly integrate the debouncer into your model, msg, and `update`
function. However, you do have to supply a global "key" with your debounce
requests, to distinguish one debouncer from another.  This is awkward -- it
seems nicer to avoid the need for globally distinct strings, by having the
client code explicitly manage the state.

So, it seems best to provide things to a debouncer by:

- including the debouncer in your model;
- delegating its messasges via your `update` function.

As far as emitting things goes, there would be a number of options. You could
imagine an extra parameter returned from an `update` function (meaning: I've
just emitted something -- here it is -- do something with it). Or, you could
imagine requiring the client of the package to provide a "mapping" function, so
that the package can emit the client's own `Msg` type. The former seems more
flexible, since it would not be difficult for the client to do its own mapping.

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
