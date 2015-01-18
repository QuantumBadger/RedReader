RedReader Beta (for Android)
============================

An unofficial, open source client for reddit.

Features
--------

* Free and Open Source - no ads/tracking.
* Swipe posts left and right to perform customisable actions, such as upvote/downvote, or save/hide.
* Advanced cache management - automatically stores past versions of posts and comments.
* Streaming - shows posts/comments as they download - good for slow connections.
* Support for multiple accounts.
* Two-column tablet mode (can be used on your phone, if it's big enough)
* Image precaching (optional: always, never, or Wi-Fi only).
* Built in image viewer, and GIF player.
* Downloads are compressed to save bandwidth.
* Night mode (i.e. a dark theme)
* Holo theme on every device
* Support for Android 2.2+ (but works best on Android 4.0+)


Reporting Bugs
--------------

RedReader is beta software, so it is possible that you may encounter bugs.

Details of each crash in the UI thread will be logged to external storage, under the filename "redreader_crash_log_(UUID).txt" - please attach the relevant log when reporting a bug.

Crashes in other parts of the application will cause a prompt to be displayed asking you to email the exception details directly.


How to Build
------------

RedReader is built using Maven. Several dependencies are required (and listed in pom.xml), but these are handled automatically if you use Maven.

Detailed instructions on building RedReader using IntelliJ IDEA are in [BUILD.md](BUILD.md).


How to Get
----------

RedReader is available for free on the Google Play store:

https://play.google.com/store/apps/details?id=org.quantumbadger.redreader


License
-------

RedReader is licensed under the GPL, version 3. A copy of the license is included in "LICENSE.txt".

Bitcoin donations are accepted at the following address: `1874wapGxDo2vEp4avisda4gx3SCjsHCQJ`


Thanks
------

Thanks to:

* Lawrence Dawson, for the [ActiveTextView](https://github.com/laurencedawson/activetextview) (Apache License 2.0)
* tomorrowkey.jp, for the [GIF decoder](https://code.google.com/p/android-gifview/) (Apache License 2.0)
* [HoloEverywhere](https://github.com/Prototik/HoloEverywhere) and [ActionBarSherlock](http://actionbarsherlock.com/)
* Apache, for various libraries
* The [Jackson JSON processor](http://jackson.codehaus.org/)
* [Joda](http://joda-time.sourceforge.net/)
* [/u/fosterbuster](http://www.reddit.com/user/fosterbuster) for the Danish translation
* [/u/balducien](http://www.reddit.com/user/balducien) and [/u/andiho](http://www.reddit.com/user/andiho) for the German translation
* [remil19](https://github.com/remil19) for the French translation
* [Husam Bilal](https://github.com/husam212) for the Arabic translation
* [Juanma Reyes](https://github.com/jmreyes) and [moshpirit](https://github.com/moshpirit) for the Spanish translation
* [Martin Macko](https://github.com/LinkedList) for the Czech translation
* [klenje](https://github.com/klenje) for the Italian translation