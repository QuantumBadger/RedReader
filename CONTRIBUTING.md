# Contributing

Pull requests for RedReader are always gratefully received. New pull requests
are reviewed whenever @QuantumBadger gets some spare time, which is usually
within a couple of weeks of submitting (apologies if it takes longer!)

## What is accepted?

Please submit pull requests for:

* :heavy_check_mark: **New features**
* :heavy_check_mark: **Bug fixes**
* :heavy_check_mark: **Significant performance improvements**
* :heavy_check_mark: **New unit tests**

Pull requests for the following things may be rejected:

* ❌ **Translations into other languages** -- please submit these using
    [Weblate](https://hosted.weblate.org/engage/redreader) to avoid conflicts!
    
* ❌ **Rewording existing English strings** -- whether the new wording is better
    or not is normally subjective. This includes README/other metadata changes.
    If you notice a typo or other mistake, please raise an issue on GitHub.
    
* ❌ **Code formatting changes** -- e.g. changing tabs to spaces, making
    trivial changes to the structure of code, or refactoring that isn't part of
    one of the accepted changes above.
    
* ❌ **Micro-optimisations** -- changes that don't have obviously significant
    performance benefits should be accompanied by benchmark results!

## Check before submitting

RedReader uses a number of automated code checks. You can run these locally
using the `pmd`, `checkstyle`, `lint`, and `test` Gradle targets:

```
./gradlew pmd checkstyle lint test
```

These checks are run automatically on every pull request, so please ensure they
pass before submitting!

## Style guidelines

For Java files, please:

* Use tabs for indentation
* Limit line length to 100 characters
* Try to match the style of existing code

For XML files, there are no restrictions on tabs vs spaces or line length, but
please try to stay consistent with the file you're editing.

For new Java or XML files:

* Add a GPLv3 copyright message to the top of the file (you can copy this from
    an existing Java or XML file)
