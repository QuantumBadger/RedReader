 <p align="left"> <img src="https://images.duckduckgo.com/iu/?u=http%3A%2F%2Ftse2.mm.bing.net%2Fth%3Fid%3DOIP.Mc9813978c3aa8d61505cfbb3ee486e61o0%26pid%3D15.1&f=1" width = "100"/> </p> RedReader Beta (for Android)
 An unofficial, open source client for reddit.

============================

![Build Status](https://travis-ci.org/QuantumBadger/RedReader.svg?branch=master)

Features
--------

* Free and Open Source Software - no ads/tracking.
* Lightweight and fast
* Downloads are compressed to save bandwidth.
* Support for multiple accounts.
* Several themes such as Night mode (i.e. dark theme) and Holo
* Swipe posts left and right to perform customisable actions, such as upvote/downvote, or save/hide.
* Advanced cache management - automatically stores past versions of posts and comments.
* Two-column tablet mode (can be used on your phone, if it's big enough)
* Image and comment precaching (optional: always, never, or Wi-Fi only).
* Built-in image viewer, imgur album viewer, GIF player, and webm/mp4/gifv player.


<p align="center">
  <img src="https://images.duckduckgo.com/iu/?u=http%3A%2F%2Fgetandroidstuff.com%2Fwp-content%2Fuploads%2F2013%2F04%2FRedReader-Beta-the-best-Reddit-Android-App.jpg&f=1" width="550"/>
</p>


How to Get
----------

RedReader is available for free on the Google Play store:

https://play.google.com/store/apps/details?id=org.quantumbadger.redreader

Redreader can also be found for free on F-Droid:

https://f-droid.org/repository/browse/?fdfilter=redreader&fdid=org.quantumbadger.redreader


Reporting Bugs
--------------

RedReader is beta software, so it is possible that you may encounter bugs.

Details of each crash in the UI thread will be logged to external storage, under the filename `redreader_crash_log_(UUID).txt` - please attach the relevant log when reporting a bug.

Crashes in other parts of the application will cause a prompt to be displayed asking you to email the exception details directly.


How to Build
------------

RedReader is built using Gradle. Several dependencies are required (and listed in build.gradle), but these are handled automatically if you use Gradle.

Detailed instructions on building RedReader using IntelliJ IDEA are in [BUILD.md](BUILD.md).


License
-------

RedReader is licensed under the GPL, version 3. A copy of the license is included in [LICENSE.txt](LICENSE.txt).

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
