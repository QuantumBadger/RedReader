# Contributing

Pull requests for RedReader are always gratefully received. New pull requests
are reviewed whenever @QuantumBadger gets some spare time, which is usually
within a couple of weeks of submitting (apologies if it takes longer!)

## Getting a Reddit API key

An API key is required to connect to Reddit. It's possible to acquire this from the
following link:

https://old.reddit.com/prefs/apps

Create an app with the type "Installed app" at the above page, and note the ID.
Also set the Redirect URI to "redreader://rr_oauth_redir".

You can set the ID at build time, or at runtime.

### At build time

* Copy the file [src/main/assets/reddit_auth.placeholder.txt] to [src/main/assets/reddit_auth.txt]
* Put your ID inside [src/main/assets/reddit_auth.txt]

Be careful not to commit this file to git, as this could expose your key.

### At runtime

Go to `Settings > Network > Reddit client ID override` and enter your ID there.

### GitHub Actions builds

In your fork, add a repository secret named `REDDIT_CLIENT_ID` under
`Settings > Secrets and variables > Actions`. The CI workflow writes it to
`src/main/assets/reddit_auth.txt` before building both APK variants. The ID is
embedded in the APK; GitHub Secrets keeps it out of the source repository and logs,
but does not make it inaccessible to someone inspecting the APK.

CI runs on pushes, pull requests and manual `workflow_dispatch` runs, with no
scheduled runs. Without the secret (including pull requests from forks), builds
still work; configure the client ID in the app before accessing Reddit.

Download `RedReader-release` or `RedReader-debug.apk` from the run's artifacts.
The Release APK is optimized but unsigned; sign it with your own release key
before installation. The Debug APK is signed with the runner's debug key.

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
