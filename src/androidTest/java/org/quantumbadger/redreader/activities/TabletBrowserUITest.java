/*******************************************************************************
 * This file is part of RedReader.
 *
 * RedReader is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RedReader is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RedReader.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.quantumbadger.redreader.activities;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import android.app.Activity;
import android.app.Instrumentation;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.os.SystemClock;
import android.view.Menu;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.webkit.WebView;
import android.widget.FrameLayout;

import androidx.appcompat.widget.PopupMenu;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Lifecycle;
import androidx.test.core.app.ActivityScenario;
import androidx.test.core.app.ApplicationProvider;
import androidx.test.ext.junit.runners.AndroidJUnit4;
import androidx.test.filters.LargeTest;
import androidx.test.filters.SdkSuppress;
import androidx.test.platform.app.InstrumentationRegistry;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.common.FeatureFlagHandler;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.SharedPrefsWrapper;
import org.quantumbadger.redreader.common.UriString;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.fragments.WebViewFragment;
import org.quantumbadger.redreader.reddit.kthings.JsonUtils;
import org.quantumbadger.redreader.reddit.kthings.RedditPost;
import org.quantumbadger.redreader.reddit.kthings.RedditThing;
import org.quantumbadger.redreader.reddit.prepared.RedditParsedPost;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.url.SubredditPostListURL;
import org.quantumbadger.redreader.views.webview.VideoEnabledWebChromeClient;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

/** Tests the tablet browser without waiting for Reddit or a remote web page. */
@LargeTest
@RunWith(AndroidJUnit4.class)
public class TabletBrowserUITest {

	private static final UriString WEB_URL = new UriString("https://example.org/article");
	private static final UriString LOCAL_URL
			= new UriString("file:///android_asset/license.html");

	private Context mContext;
	private SharedPrefsWrapper mPrefs;

	@Before
	public void setUp() throws Exception {
		mContext = ApplicationProvider.getApplicationContext();
		mPrefs = General.getSharedPrefs(mContext);
		final SharedPreferences rawPrefs = mContext.getSharedPreferences(
				mContext.getPackageName() + "_preferences", Context.MODE_PRIVATE);

		PrefsUtility.acceptRedditUserAgreement();
		@SuppressWarnings("deprecation")
		final int version = mContext.getPackageManager()
				.getPackageInfo(mContext.getPackageName(), 0).versionCode;

		if(!rawPrefs.contains(FeatureFlagHandler.PREF_FIRST_RUN_MESSAGE_SHOWN)
				|| rawPrefs.getInt(FeatureFlagHandler.PREF_LAST_VERSION, 0) != version) {
			FeatureFlagHandler.handleFirstInstall(mPrefs);
			mPrefs.edit()
					.putString(FeatureFlagHandler.PREF_FIRST_RUN_MESSAGE_SHOWN, "true")
					.putInt(FeatureFlagHandler.PREF_LAST_VERSION, version)
					.apply();
		}

		mPrefs.edit()
				.putString(mContext.getString(R.string.pref_appearance_twopane_key), "force")
				.putBoolean("pref_behaviour_back_again", false)
				.putBoolean(mContext.getString(R.string.pref_behaviour_skiptofrontpage_key), false)
				.putBoolean(mContext.getString(R.string.pref_behaviour_useinternalbrowser_key), true)
				.putBoolean(mContext.getString(R.string.pref_behaviour_usecustomtabs_key), false)
				.apply();
		settlePreferences();
	}

	@Test
	public void ordinaryLinkKeepsListingAndBackRestoresMenu() {
		try(ActivityScenario<MainActivity> scenario = launchWithListing()) {
			final AtomicReference<View> menu = new AtomicReference<>();
			final AtomicReference<View> listing = new AtomicReference<>();
			scenario.onActivity(activity -> {
				menu.set(leftPane(activity).getChildAt(0));
				listing.set(rightPane(activity).getChildAt(0));
				LinkHandler.onLinkClicked(activity, WEB_URL, false);
				blockBrowserNetwork(activity);
				assertSame(listing.get(), leftPane(activity).getChildAt(0));
				assertNotNull(findWebView(rightPane(activity)));
				assertTrue(activity.getOnBackPressedDispatcher().hasEnabledCallbacks());
			});

			dispatchBack(scenario);

			scenario.onActivity(activity -> {
				assertSame(menu.get(), leftPane(activity).getChildAt(0));
				assertSame(listing.get(), rightPane(activity).getChildAt(0));
				assertNull(findWebView(rightPane(activity)));
			});
			assertEquals(Lifecycle.State.RESUMED, scenario.getState());
		}
	}

	@Test
	public void selectingAnotherLinkReplacesBrowserWithoutStackingPanes() {
		try(ActivityScenario<MainActivity> scenario = launchWithListing()) {
			final AtomicReference<View> listing = new AtomicReference<>();
			final AtomicReference<WebView> firstBrowser = new AtomicReference<>();
			scenario.onActivity(activity -> {
				listing.set(rightPane(activity).getChildAt(0));
				assertTrue(activity.openInternalBrowser(LOCAL_URL, null));
				firstBrowser.set(findWebView(rightPane(activity)));
				assertNotNull(firstBrowser.get());
				assertTrue(activity.openInternalBrowser(WEB_URL, null));
				blockBrowserNetwork(activity);
				assertNotSame(firstBrowser.get(), findWebView(rightPane(activity)));
				assertSame(listing.get(), leftPane(activity).getChildAt(0));
				assertEquals(1, rightPane(activity).getChildCount());
				assertEquals(1, browserFragmentCount(activity));
			});

			dispatchBack(scenario);

			scenario.onActivity(activity -> {
				assertSame(listing.get(), rightPane(activity).getChildAt(0));
				assertEquals(0, browserFragmentCount(activity));
			});
		}
	}

	@Test
	public void backNavigatesWebHistoryBeforeClosingPane() {
		try(ActivityScenario<MainActivity> scenario = launchWithListing()) {
			scenario.onActivity(activity ->
					assertTrue(activity.openInternalBrowser(LOCAL_URL, null)));
			awaitBrowserState(scenario, false, LOCAL_URL.value);

			scenario.onActivity(activity -> findWebView(rightPane(activity)).loadDataWithBaseURL(
					"https://example.invalid/second",
					"<html><body>Second local page</body></html>",
					"text/html", "UTF-8", null));
			awaitBrowserState(scenario, true, null);

			dispatchBack(scenario);
			awaitBrowserState(scenario, false, LOCAL_URL.value);
			assertEquals(Lifecycle.State.RESUMED, scenario.getState());

			dispatchBack(scenario);
			scenario.onActivity(activity -> assertNull(findWebView(rightPane(activity))));
			assertEquals(Lifecycle.State.RESUMED, scenario.getState());
		}
	}

	@Test
	public void closingBrowserOpenedFromCommentsRestoresSameComments() throws Exception {
		final RedditPost post = fixturePost();
		try(ActivityScenario<MainActivity> scenario = launchWithListing()) {
			final AtomicReference<View> comments = new AtomicReference<>();
			scenario.onActivity(activity -> {
				activity.onPostCommentsSelected(preparePost(activity, post));
				comments.set(rightPane(activity).getChildAt(0));
				assertTrue(activity.openInternalBrowser(LOCAL_URL, post));
				assertNotNull(findWebView(rightPane(activity)));
				activity.onBrowserCloseRequested();
				assertSame(comments.get(), rightPane(activity).getChildAt(0));
				assertNull(findWebView(rightPane(activity)));
			});

			dispatchBack(scenario);
			scenario.onActivity(activity ->
					assertNotSame(comments.get(), rightPane(activity).getChildAt(0)));
		}
	}

	@Test
	public void selectingCommentsReplacesBrowser() throws Exception {
		final RedditPost post = fixturePost();
		try(ActivityScenario<MainActivity> scenario = launchWithListing()) {
			scenario.onActivity(activity -> {
				final View listing = rightPane(activity).getChildAt(0);
				assertTrue(activity.openInternalBrowser(LOCAL_URL, null));
				activity.onPostCommentsSelected(preparePost(activity, post));
				assertSame(listing, leftPane(activity).getChildAt(0));
				assertNotNull(rightPane(activity).getChildAt(0));
				assertNull(findWebView(rightPane(activity)));
				assertEquals(0, browserFragmentCount(activity));
			});
		}
	}

	@Test
	public void recreatingActivityRestoresBrowserAndMenuContext() {
		try(ActivityScenario<MainActivity> scenario = launchWithListing()) {
			scenario.onActivity(activity ->
					assertTrue(activity.openInternalBrowser(LOCAL_URL, null)));
			awaitBrowserState(scenario, false, LOCAL_URL.value);
			scenario.recreate();
			awaitBrowserState(scenario, false, LOCAL_URL.value);
			scenario.onActivity(activity -> {
				assertNotNull(leftPane(activity).getChildAt(0));
				assertNull(findWebView(leftPane(activity)));
				assertEquals(1, browserFragmentCount(activity));
			});

			dispatchBack(scenario);
			scenario.onActivity(activity -> {
				assertNotNull(leftPane(activity).getChildAt(0));
				assertNotNull(rightPane(activity).getChildAt(0));
				assertEquals(0, browserFragmentCount(activity));
			});
			assertEquals(Lifecycle.State.RESUMED, scenario.getState());
		}
	}

	@Test
	public void recreatingActivityRestoresBrowserAndCommentsContext() throws Exception {
		final RedditPost post = fixturePost();
		try(ActivityScenario<MainActivity> scenario = launchWithListing()) {
			scenario.onActivity(activity -> {
				activity.onPostCommentsSelected(preparePost(activity, post));
				assertTrue(activity.openInternalBrowser(LOCAL_URL, post));
			});
			awaitBrowserState(scenario, false, LOCAL_URL.value);
			scenario.recreate();
			awaitBrowserState(scenario, false, LOCAL_URL.value);

			dispatchBack(scenario);
			final AtomicReference<View> comments = new AtomicReference<>();
			scenario.onActivity(activity -> {
				assertEquals(0, browserFragmentCount(activity));
				assertNotNull(leftPane(activity).getChildAt(0));
				comments.set(rightPane(activity).getChildAt(0));
				assertNotNull(comments.get());
				assertTrue(activity.getOnBackPressedDispatcher().hasEnabledCallbacks());
			});

			dispatchBack(scenario);
			scenario.onActivity(activity ->
					assertNotSame(comments.get(), rightPane(activity).getChildAt(0)));
			assertEquals(Lifecycle.State.RESUMED, scenario.getState());
		}
	}

	@Test
	public void refreshingListingsKeepsBrowserInRightPane() throws Exception {
		final RedditPost post = fixturePost();
		try(ActivityScenario<MainActivity> scenario = launchWithListing()) {
			scenario.onActivity(activity -> {
				activity.onPostCommentsSelected(preparePost(activity, post));
				assertTrue(activity.openInternalBrowser(LOCAL_URL, null));
				final WebView browser = findWebView(rightPane(activity));
				activity.requestRefresh(RefreshableActivity.RefreshableFragment.POSTS, false);
				assertSame(browser, findWebView(rightPane(activity)));
				assertNotNull(leftPane(activity).getChildAt(0));
				activity.requestRefresh(RefreshableActivity.RefreshableFragment.COMMENTS, false);
				assertSame(browser, findWebView(rightPane(activity)));
				assertEquals(1, browserFragmentCount(activity));
				activity.onBrowserCloseRequested();
				assertNotNull(rightPane(activity).getChildAt(0));
				assertNull(findWebView(rightPane(activity)));
			});
		}
	}

	@Test
	public void browserCloseWhileStateSavedWaitsForResume() {
		try(ActivityScenario<MainActivity> scenario = launchWithListing()) {
			final AtomicReference<MainActivity> activityRef = new AtomicReference<>();
			final AtomicReference<View> menu = new AtomicReference<>();
			final AtomicReference<View> listing = new AtomicReference<>();
			scenario.onActivity(activity -> {
				activityRef.set(activity);
				menu.set(leftPane(activity).getChildAt(0));
				listing.set(rightPane(activity).getChildAt(0));
				assertTrue(activity.openInternalBrowser(LOCAL_URL, null));
			});
			awaitBrowserState(scenario, false, LOCAL_URL.value);
			scenario.moveToState(Lifecycle.State.CREATED);

			InstrumentationRegistry.getInstrumentation().runOnMainSync(() -> {
				final MainActivity activity = activityRef.get();
				assertTrue(activity.getSupportFragmentManager().isStateSaved());
				activity.onBrowserCloseRequested();
				assertEquals(1, browserFragmentCount(activity));
				assertNotNull(findWebView(rightPane(activity)));
			});

			scenario.moveToState(Lifecycle.State.RESUMED);
			scenario.onActivity(activity -> {
				assertEquals(0, browserFragmentCount(activity));
				assertSame(menu.get(), leftPane(activity).getChildAt(0));
				assertSame(listing.get(), rightPane(activity).getChildAt(0));
				assertNull(findWebView(rightPane(activity)));
			});
		}
	}

	@Test
	public void openInBrowserMenuRestoresPanesWithoutFinishingActivity() {
		final Instrumentation instrumentation = InstrumentationRegistry.getInstrumentation();
		final IntentFilter filter = new IntentFilter(Intent.ACTION_VIEW);
		filter.addDataScheme("https");
		final Instrumentation.ActivityMonitor monitor = instrumentation.addMonitor(filter,
				new Instrumentation.ActivityResult(Activity.RESULT_CANCELED, null), true);
		try(ActivityScenario<MainActivity> scenario = launchWithListing()) {
			scenario.onActivity(activity -> {
				final View menuView = leftPane(activity).getChildAt(0);
				final View listing = rightPane(activity).getChildAt(0);
				assertTrue(activity.openInternalBrowser(WEB_URL, null));
				blockBrowserNetwork(activity);
				final Menu menu = new PopupMenu(activity, rightPane(activity)).getMenu();
				activity.onCreateOptionsMenu(menu);
				assertNotNull(menu.findItem(WebViewActivity.VIEW_IN_BROWSER));
				assertTrue(activity.onOptionsItemSelected(
						menu.findItem(WebViewActivity.VIEW_IN_BROWSER)));
				assertSame(menuView, leftPane(activity).getChildAt(0));
				assertSame(listing, rightPane(activity).getChildAt(0));
				assertEquals(0, browserFragmentCount(activity));
			});
			assertEquals(1, monitor.getHits());
			assertEquals(Lifecycle.State.RESUMED, scenario.getState());
		} finally {
			instrumentation.removeMonitor(monitor);
		}
	}

	@Test
	@SdkSuppress(minSdkVersion = 26)
	public void fullscreenBackAndCloseRestoreActivityWindow() {
		try(ActivityScenario<MainActivity> scenario = launchWithListing()) {
			final AtomicReference<WebView> browserRef = new AtomicReference<>();
			scenario.onActivity(activity -> {
				assertTrue(activity.openInternalBrowser(LOCAL_URL, null));
				final WebView browser = findWebView(rightPane(activity));
				browserRef.set(browser);
				final VideoEnabledWebChromeClient client
						= (VideoEnabledWebChromeClient)browser.getWebChromeClient();
				client.onShowCustomView(new FrameLayout(activity), () -> {});
				assertTrue(client.isVideoFullscreen());
				assertFalse(activity.getSupportActionBar().isShowing());
			});

			dispatchBack(scenario);
			scenario.onActivity(activity -> {
				assertSame(browserRef.get(), findWebView(rightPane(activity)));
				assertTrue(activity.getSupportActionBar().isShowing());
				final VideoEnabledWebChromeClient client
						= (VideoEnabledWebChromeClient)browserRef.get().getWebChromeClient();
				assertFalse(client.isVideoFullscreen());
				client.onShowCustomView(new FrameLayout(activity), () -> {});
				assertFalse(activity.getSupportActionBar().isShowing());
				activity.onBrowserCloseRequested();
				assertTrue(activity.getSupportActionBar().isShowing());
				assertEquals(0, activity.getWindow().getAttributes().flags
						& WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
				assertNull(findWebView(rightPane(activity)));
			});
			assertEquals(Lifecycle.State.RESUMED, scenario.getState());
		}
	}

	@Test
	public void externalBrowserPreferenceStillLaunchesExternalActivity() {
		mPrefs.edit().putBoolean(
				mContext.getString(R.string.pref_behaviour_useinternalbrowser_key), false).apply();
		settlePreferences();
		assertExternalBrowserRoute();
	}

	@Test
	public void customTabsPreferenceStillLaunchesExternalActivity() {
		mPrefs.edit().putBoolean(
				mContext.getString(R.string.pref_behaviour_usecustomtabs_key), true).apply();
		settlePreferences();
		assertExternalBrowserRoute();
	}

	@Test
	public void singlePaneUsesSeparateBrowserActivity() {
		mPrefs.edit().putString(
				mContext.getString(R.string.pref_appearance_twopane_key), "never").apply();
		settlePreferences();
		final Instrumentation instrumentation = InstrumentationRegistry.getInstrumentation();
		final Instrumentation.ActivityMonitor monitor = instrumentation.addMonitor(
				WebViewActivity.class.getName(),
				new Instrumentation.ActivityResult(Activity.RESULT_CANCELED, null), true);
		try(ActivityScenario<MainActivity> scenario = ActivityScenario.launch(MainActivity.class)) {
			scenario.onActivity(activity -> {
				assertFalse(activity.openInternalBrowser(LOCAL_URL, null));
				LinkHandler.onLinkClicked(activity, WEB_URL, false);
				assertEquals(0, browserFragmentCount(activity));
			});
			assertEquals(1, monitor.getHits());
		} finally {
			instrumentation.removeMonitor(monitor);
		}
	}

	private void assertExternalBrowserRoute() {
		final Instrumentation instrumentation = InstrumentationRegistry.getInstrumentation();
		final IntentFilter filter = new IntentFilter(Intent.ACTION_VIEW);
		filter.addDataScheme("https");
		final Instrumentation.ActivityMonitor monitor = instrumentation.addMonitor(filter,
				new Instrumentation.ActivityResult(Activity.RESULT_CANCELED, null), true);
		try(ActivityScenario<MainActivity> scenario = launchWithListing()) {
			scenario.onActivity(activity -> {
				final View listing = rightPane(activity).getChildAt(0);
				LinkHandler.onLinkClicked(activity, WEB_URL, false);
				assertSame(listing, rightPane(activity).getChildAt(0));
				assertEquals(0, browserFragmentCount(activity));
			});
			assertEquals(1, monitor.getHits());
		} finally {
			instrumentation.removeMonitor(monitor);
		}
	}

	private static ActivityScenario<MainActivity> launchWithListing() {
		final ActivityScenario<MainActivity> scenario = ActivityScenario.launch(MainActivity.class);
		scenario.onActivity(activity -> {
			activity.onSelected(SubredditPostListURL.getFrontPage());
			assertNotNull(rightPane(activity).getChildAt(0));
		});
		return scenario;
	}

	private static ViewGroup leftPane(final MainActivity activity) {
		return activity.findViewById(R.id.main_left_frame);
	}

	private static ViewGroup rightPane(final MainActivity activity) {
		return activity.findViewById(R.id.main_right_frame);
	}

	private static WebView findWebView(final View view) {
		if(view instanceof WebView) {
			return (WebView)view;
		}
		if(view instanceof ViewGroup) {
			final ViewGroup group = (ViewGroup)view;
			for(int i = 0; i < group.getChildCount(); i++) {
				final WebView browser = findWebView(group.getChildAt(i));
				if(browser != null) {
					return browser;
				}
			}
		}
		return null;
	}

	private static void blockBrowserNetwork(final MainActivity activity) {
		final WebView browser = findWebView(rightPane(activity));
		assertNotNull(browser);
		browser.getSettings().setBlockNetworkLoads(true);
	}

	private static int browserFragmentCount(final MainActivity activity) {
		int count = 0;
		for(final Fragment fragment : activity.getSupportFragmentManager().getFragments()) {
			if(fragment instanceof WebViewFragment) {
				count++;
			}
		}
		return count;
	}

	private static void dispatchBack(final ActivityScenario<MainActivity> scenario) {
		// BaseActivity debounces back presses within 300ms on older Android versions.
		SystemClock.sleep(400);
		scenario.onActivity(activity -> activity.getOnBackPressedDispatcher().onBackPressed());
		InstrumentationRegistry.getInstrumentation().waitForIdleSync();
	}

	private static void settlePreferences() {
		// Drain asynchronous preference callbacks before constructing MainActivity's panes.
		SystemClock.sleep(300);
		InstrumentationRegistry.getInstrumentation().waitForIdleSync();
	}

	private static void awaitBrowserState(
			final ActivityScenario<MainActivity> scenario,
			final boolean hasHistory,
			final String expectedUrl) {
		final AtomicBoolean ready = new AtomicBoolean();
		for(int attempt = 0; attempt < 100; attempt++) {
			scenario.onActivity(activity -> {
				final WebView browser = findWebView(rightPane(activity));
				ready.set(browser != null && browser.getProgress() == 100
						&& browser.canGoBack() == hasHistory
						&& (expectedUrl == null || expectedUrl.equals(browser.getUrl())));
			});
			if(ready.get()) {
				return;
			}
			SystemClock.sleep(100);
		}
		assertTrue("Browser did not reach the expected local page and history state", ready.get());
	}

	private static RedditPost fixturePost() throws Exception {
		final String json = "{\"kind\":\"t3\",\"data\":{"
				+ "\"id\":\"abc123\",\"name\":\"t3_abc123\","
				+ "\"url\":\"https://example.org/article\",\"title\":\"Test post\","
				+ "\"author\":\"test\",\"subreddit\":\"test\",\"domain\":\"example.org\","
				+ "\"num_comments\":0,\"score\":1,\"created_utc\":1700000000,"
				+ "\"permalink\":\"/r/test/comments/abc123/test_post/\"}}";
		return ((RedditThing.Post)JsonUtils.INSTANCE.decodeRedditThingFromStream(
				new ByteArrayInputStream(json.getBytes(StandardCharsets.UTF_8)))).getData();
	}

	private static RedditPreparedPost preparePost(
			final MainActivity activity, final RedditPost post) {
		return new RedditPreparedPost(activity, CacheManager.getInstance(activity), 0,
				new RedditParsedPost(activity, post, false), TimestampUTC.ZERO,
				false, false, false, false);
	}
}
