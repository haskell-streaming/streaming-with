# Revision history for streaming-with

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
