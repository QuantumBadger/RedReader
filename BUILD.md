How to Build RedReader
======================

**1\. Download and install the Java JDK.**

**2\. Download and install the Android SDK.**

Go to this page:

http://developer.android.com/sdk/index.html#download

And download the installer under "Use an existing IDE".

When it's installed, go into the SDK Manager, and install the necessary SDK Platform, build tools, and support repository. If you wish to use the emulator, install the relevant system images.

**3\. Download and install IntelliJ IDEA Community Edition.**

http://www.jetbrains.com/idea/download/index.html

**4\. Download and install Git.**

For Windows, download the installer from here: http://git-scm.com/. When asked about line ending conversions, choose "Checkout Windows-style, commit Unix-style line endings".

**5\. Configure IDEA to use Git.**

Open IDEA. At the start screen, select "Configure", then "Settings". Expand the "Version Control" section (in the list on the left), and select "Git". Then, tell it where the Git executable is (on Windows, this is under Program Files/Git/bin/git.exe).

Press OK to return to the "Configure" screen, and press the back arrow to return to the start screen.

**6\. Get the RedReader source.**

Using GitHub, fork the RedReader repo. Then, at the IDEA start screen, select "Check out from Version Control", then "GitHub". Login, and specify a password for the IDEA password database.

Select the relevant repo, create the parent directory, and select "Clone".

**7\. Configure IDEA to use the JDK.**

Go to "File > Project Structure". Select "SDKs", under "Platform Settings". Click the green add icon, and select "JDK". Locate the JDK home directory (under Windows this is /Program Files/Java/jdk1.7.0_XX).

**8\. Configure IDEA to use the Android SDK.**

In the same dialog, click the green add icon again, but this time select "Android SDK".

Locate the Android SDK home directory. On Windows, this could be in a variety of locations - the easiest way to find out is to open the SDK manager and look for the "SDK Path" in gray at the top of the window. In the IDEA file selector, you'll probably need to click the "show hidden files and directories" icon.

Once you've done this, select the relevant version of Android as the build target in the next dialog.

**9\. Create a run configuration.**

Open the Run menu, and select "Edit Configurations". Press the green add icon, and select "Android Application".

* Under Module, select "redreader".
* Specify "MainActivity" as the activity to launch (select the radio button next to "Launch:", click the browser button ("..."), and select "MainActivity" from the list.
* Select "Show chooser dialog" under "Target Device".
* Under "Name" (at the top), type whatever you like.
* Press OK.

**10\. Voilà, you're done!**

Click the green "run" button in the toolbar to build and run RedReader. You'll probably need to uninstall the Google Play copy from your device, or you'll get a certificate conflict.

**Tips**

* It'll take a while to build the first time. On subsequent builds, it takes about 10 seconds on my machine if I change the Java code, and about 40 if I change the XML.
* Use Logcat to inspect error messages.
* Don't bother using the debugger, in my experience it slows the HTTP requests down to a crawl, and it can take forever to do simple things.
* After adding strings or arrays to the XML files, rebuild the whole project (Build > Rebuild project) - otherwise, the strings get mixed up (for some reason).
* Commit to the repo using "VCS > Commit Changes". Don't forget to also push the changes to GitHub.
