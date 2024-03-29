# Revision history for streaming-with

## 0.3.0.0 -- 2021-08-29

* Support newer version of `streaming-bytestring` and GHC.

## 0.2.2.1 -- 2018-05-23

* Bump temporary dependency

## 0.2.2.0 -- 2018-05-23

* Actually export `liftThrow` from within `Streaming.With.Lifted`.

## 0.2.1.1 -- 2018-03-23

* Bump exceptions dependency

## 0.2.1.0 -- 2018-02-06

* Add the `RunWithable` class.

* Add the `within`, `liftActionIO` and `liftThrow` functions for use
  with `Withable`.

## 0.2.0.0 -- 2017-07-07

* Add `Monad w` constraint to `Withable w`

    - This was the intended use case all along, with the intent that
      you can write code generic in the choice of `w`.

## 0.1.1.0 -- 2017-07-07

* Add support for the [temporary] package, specifically the functions:

    * `withSystemTempFile`
    * `withTempFile`
    * `withSystemTempDirectory`
    * `withTempDirectory`

    [temporary]: http://hackage.haskell.org/package/temporary

## 0.1.0.0 -- 2017-06-30

* First release.
