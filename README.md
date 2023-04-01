# artisanal-servant-exceptions

When a Servant handler throws an exception, we would like to have information
about the chain of invocations that led to the exception. At that point we dont
want to *recover* from the exception, but we would like to log it, providing as
much context as possible.

This repo is an example of a possible approach. If you run the server with
`cabal run` and invoke the endpoint with

```
curl -v -X POST localhost:8000
```

then the following exception will be logged:

```
ExceptionWithStackTrace ["runFoo","runBar","runBaz"] user error (some exception)
```

Where `runFoo`, `runBar` and `runBaz` are annotations identifying functions in
the call stack. Those annotations need to be explicitly added, they're not
automatic. On the other hand, no changes to exception-throwing or
exception-catching code are needed.

## How it works

For each individual request, we allocate an `IORef` which will contain the stack
trace (a list of annotations). That `IORef` is passed as a reader environment.
This is done in Servant's `hoistServer` function.

When we enter an annotated function, we empty the `IORef`. We do the same when
we exit an annotated function normally. 

When we exit an annotated function _abnormally_, because of an exception, we add
the function's annotation into the `IORef`. As the exception bubbles up, we
build a chain of annotations.

Once we are out of the handler, we catch the exception, inspect the `IORef`'s
contents, and re-throw the exception wrapped in an `ExceptionWithStackTrace`.

### A problem with this approach

If we catch an exception coming from an annotated function and then throw an
unrelated exception _without crossing an annotation frontier_ the old exception
stays in the stack trace, which might be misleading.

## Possible alternatives

We could store the stack trace directly in the reader environment, without using
a mutable `IORef`. 

The stack trace would grow (using `local`) as we get into nested function calls.
Exceptions would be caught and re-thrown as `ExceptionWithStackTrace` at the
_earliest_ possible moment, without waiting until we exit the Servant handler.

The problem with this is that it might interfere with exception-catching code.
If one layer throws `IOException`s and some upper layer relies con catching
them, converting the `IOException`s into `ExceptionWithStackTrace`s in some
intermediate layer will break the logic.

