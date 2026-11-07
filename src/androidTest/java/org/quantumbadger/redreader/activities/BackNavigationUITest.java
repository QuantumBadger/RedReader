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

import static androidx.test.espresso.Espresso.onView;
import static androidx.test.espresso.Espresso.pressBackUnconditionally;
import static androidx.test.espresso.action.ViewActions.click;
import static androidx.test.espresso.assertion.ViewAssertions.matches;
import static androidx.test.espresso.matcher.ViewMatchers.isDisplayed;
import static androidx.test.espresso.matcher.ViewMatchers.isRoot;
import static androidx.test.espresso.matcher.ViewMatchers.withId;
import static androidx.test.espresso.matcher.ViewMatchers.withText;

import static org.hamcrest.Matchers.allOf;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;

import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Build;
import android.os.SystemClock;
import android.view.View;
import android.webkit.WebView;
import android.window.OnBackInvokedCallback;
import android.window.OnBackInvokedDispatcher;

import androidx.lifecycle.Lifecycle;
import androidx.test.core.app.ActivityScenario;
import androidx.test.core.app.ApplicationProvider;
import androidx.test.ext.junit.runners.AndroidJUnit4;
import androidx.test.filters.LargeTest;
import androidx.test.platform.app.InstrumentationRegistry;

import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.hamcrest.TypeSafeMatcher;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.FeatureFlagHandler;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.SharedPrefsWrapper;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Tests for the predictive back migration (API 36 / Android 16).
 *
 * <p>All {@code onBackPressed()} overrides have been removed from the app, so
 * any correct back behaviour observed here must be flowing through
 * {@code BaseActivity}'s {@code OnBackPressedCallback}.
 */
@LargeTest
@RunWith(AndroidJUnit4.class)
public class BackNavigationUITest {

	private static final String TEST_SUBREDDIT_URL
			= "https://reddit.com/r/redreader_public_test";

	private static final String PREF_BACK_AGAIN = "pref_behaviour_back_again";
	private static final String PREF_TWOPANE = "pref_appearance_twopane";
	private static final String PREF_POST_TAP_ACTION = "pref_behaviour_post_tap_action";

	private Context mContext;
	private SharedPrefsWrapper mPrefs;
	private android.content.SharedPreferences mRawPrefs;

	@Before
	public void setUp() throws Exception {
		mContext = ApplicationProvider.getApplicationContext();
		mPrefs = General.getSharedPrefs(mContext);
		mRawPrefs = mContext.getSharedPreferences(
				mContext.getPackageName() + "_preferences",
				Context.MODE_PRIVATE);

		// Stop the terms screen, the first-run login prompt and the changelog
		// dialog from covering the activities under test
		PrefsUtility.acceptRedditUserAgreement();

		final int versionCode = (int)mContext.getPackageManager()
				.getPackageInfo(mContext.getPackageName(), 0)
				.getLongVersionCode();

		if (!mRawPrefs.contains(FeatureFlagHandler.PREF_FIRST_RUN_MESSAGE_SHOWN)
				|| mRawPrefs.getInt(FeatureFlagHandler.PREF_LAST_VERSION, 0)
						!= versionCode) {

			// Marking the first run as done skips MainActivity's call to
			// handleFirstInstall(), so the feature flags must be set here
			// instead. Without this, handleUpgrade() writes preferences during
			// onCreate(), which triggers a refresh before the layout exists.
			FeatureFlagHandler.handleFirstInstall(mPrefs);

			mPrefs.edit()
					.putString(FeatureFlagHandler.PREF_FIRST_RUN_MESSAGE_SHOWN, "true")
					.putInt(FeatureFlagHandler.PREF_LAST_VERSION, versionCode)
					.apply();

			settlePreferences();
		}

		setBackAgain(false);
		setTwoPane("auto");
		setPostTapAction("link");
	}

	private void setBackAgain(final boolean value) {

		if (mRawPrefs.getBoolean(PREF_BACK_AGAIN, false) != value) {
			mPrefs.edit().putBoolean(PREF_BACK_AGAIN, value).apply();
			settlePreferences();
		}
	}

	private void setTwoPane(final String value) {

		if (!value.equals(mRawPrefs.getString(PREF_TWOPANE, "auto"))) {
			mPrefs.edit().putString(PREF_TWOPANE, value).apply();
			settlePreferences();
		}
	}

	private void setPostTapAction(final String value) {

		if (!value.equals(mRawPrefs.getString(PREF_POST_TAP_ACTION, "link"))) {
			mPrefs.edit().putString(PREF_POST_TAP_ACTION, value).apply();
			settlePreferences();
		}
	}

	/**
	 * {@code apply()} delivers its listener callbacks asynchronously on the main
	 * thread. Activities refresh themselves when preferences change, so the
	 * callbacks must be drained before an activity is launched -- otherwise they
	 * arrive midway through {@code onCreate()}, before the activity has finished
	 * building its layout.
	 */
	private static void settlePreferences() {
		SystemClock.sleep(300);
		waitForIdle();
	}

	private static Intent intentFor(
			final Context context,
			final Class<?> activity,
			final String url) {

		final Intent intent = new Intent(context, activity);
		intent.setData(Uri.parse(url));
		return intent;
	}

	/**
	 * Whether the activity is currently intercepting back presses. When this is
	 * false on Android 16, the system handles back itself (with predictive back
	 * animations).
	 */
	private static boolean hasEnabledCallbacks(final ActivityScenario<?> scenario) {

		final AtomicBoolean result = new AtomicBoolean();

		scenario.onActivity(activity -> result.set(
				((BaseActivity)activity).getOnBackPressedDispatcher()
						.hasEnabledCallbacks()));

		return result.get();
	}

	/**
	 * Asserts whether the activity is intercepting back presses.
	 *
	 * <p>Below API 36 the callback is always enabled, because it also runs the
	 * double-press guard; {@code expectedOnApi36} therefore only applies where
	 * the OS provides predictive back.
	 */
	private static void assertIntercepts(
			final String message,
			final boolean expectedOnApi36,
			final ActivityScenario<?> scenario) {

		assertEquals(
				message,
				Build.VERSION.SDK_INT < 36 || expectedOnApi36,
				hasEnabledCallbacks(scenario));
	}

	/**
	 * Presses back, having first waited out the 300ms double-press guard. Below
	 * API 36 the guard is active and Espresso's back press is fast enough to
	 * fall inside its window.
	 */
	private static void pressBackAfterGuardWindow() {
		SystemClock.sleep(400);
		pressBackUnconditionally();
	}

	/**
	 * Polls until a view matching {@code matcher} is displayed, or the timeout
	 * expires. Used in place of a fixed wait for content which is loaded over
	 * the network.
	 */
	private static void awaitView(final Matcher<View> matcher, final int timeoutSeconds) {

		for (int attempt = 0; attempt < timeoutSeconds * 2; attempt++) {

			try {
				onView(firstMatching(allOf(matcher, isDisplayed())))
						.check(matches(isDisplayed()));
				return;

			} catch (final RuntimeException | AssertionError e) {
				SystemClock.sleep(500);
			}
		}
	}

	private static void waitForIdle() {
		InstrumentationRegistry.getInstrumentation().waitForIdleSync();
	}

	/**
	 * {@code finish()} is asynchronous, so an activity that is on its way out
	 * briefly reports STARTED rather than DESTROYED.
	 */
	private static Lifecycle.State awaitDestroyed(final ActivityScenario<?> scenario) {

		for (int attempt = 0; attempt < 100; attempt++) {

			if (scenario.getState() == Lifecycle.State.DESTROYED) {
				return Lifecycle.State.DESTROYED;
			}

			SystemClock.sleep(50);
		}

		return scenario.getState();
	}

	/**
	 * Dispatches a back press straight through the activity's
	 * {@code OnBackPressedDispatcher}, which is where the double-press guard
	 * lives.
	 *
	 * <p>Espresso's {@code pressBack()} takes the better part of a second to
	 * complete, so it cannot be used to produce two presses inside the 300ms
	 * guard window. The end-to-end tests in this class do use real input; this
	 * helper exists only for the tests that need presses in quick succession.
	 */
	private static void dispatchBack(final ActivityScenario<?> scenario) {
		scenario.onActivity(activity ->
				((BaseActivity)activity).getOnBackPressedDispatcher().onBackPressed());
	}

	// ---------------------------------------------------------------------
	// Premise: predictive back really is active for this app on this device
	// ---------------------------------------------------------------------

	/**
	 * The whole migration only matters if the platform has actually enabled
	 * the ahead-of-time back dispatch for us. If this fails, every other test
	 * in this class would be passing via the legacy key-event path.
	 *
	 * <p>Registering a platform {@link OnBackInvokedCallback} and observing it
	 * fire proves the ahead-of-time dispatch is live: when it is disabled, the
	 * platform dispatcher never invokes registered callbacks and back arrives
	 * as a {@code KEYCODE_BACK} key event instead.
	 */
	@Test
	public void predictiveBackIsEnabledExactlyOnApi36AndAbove() {

		try (ActivityScenario<HtmlViewActivity> scenario
					 = ActivityScenario.launch(htmlIntent(mContext, HTML_NO_HISTORY))) {

			waitForIdle();

			final AtomicBoolean invoked = new AtomicBoolean(false);
			final AtomicReference<OnBackInvokedCallback> callbackRef
					= new AtomicReference<>();

			scenario.onActivity(activity -> {
				final OnBackInvokedCallback callback = () -> invoked.set(true);
				callbackRef.set(callback);
				activity.getOnBackInvokedDispatcher().registerOnBackInvokedCallback(
						OnBackInvokedDispatcher.PRIORITY_OVERLAY,
						callback);
			});

			waitForIdle();

			pressBackUnconditionally();
			waitForIdle();

			// This is exactly the condition BaseActivity.osHandlesBackAnimations()
			// tests, so this asserts that predicate is correct on this device.
			assertEquals(
					"Predictive back should be enabled precisely when the app "
							+ "targets API 36 and the device is API 36+",
					Build.VERSION.SDK_INT >= 36,
					invoked.get());

			if (invoked.get()) {
				scenario.onActivity(activity ->
						activity.getOnBackInvokedDispatcher()
								.unregisterOnBackInvokedCallback(callbackRef.get()));
			}
		}
	}

	// ---------------------------------------------------------------------
	// PostListingActivity: "press back again to exit"
	// ---------------------------------------------------------------------

	@Test
	public void postListing_backAgainDisabled_doesNotIntercept() {

		setBackAgain(false);

		try (ActivityScenario<PostListingActivity> scenario = ActivityScenario.launch(
				intentFor(mContext, PostListingActivity.class, TEST_SUBREDDIT_URL))) {

			waitForIdle();

			// Nothing to intercept, so the system should own the back gesture
			// (and therefore animate it).
			assertIntercepts(
					"PostListingActivity should not intercept back when the "
							+ "'back again' pref is disabled",
					false,
					scenario);

			pressBackUnconditionally();
			waitForIdle();

			assertEquals(
					"A single back press should exit",
					Lifecycle.State.DESTROYED,
					awaitDestroyed(scenario));
		}
	}

	@Test
	public void postListing_backAgainEnabled_requiresTwoPresses() {

		setBackAgain(true);

		try (ActivityScenario<PostListingActivity> scenario = ActivityScenario.launch(
				intentFor(mContext, PostListingActivity.class, TEST_SUBREDDIT_URL))) {

			waitForIdle();

			assertIntercepts(
					"PostListingActivity must intercept back to show the "
							+ "'press back again' toast",
					true,
					scenario);

			pressBackUnconditionally();
			waitForIdle();

			assertEquals(
					"The first back press should be consumed by the "
							+ "'press back again' prompt",
					Lifecycle.State.RESUMED,
					scenario.getState());

			pressBackAfterGuardWindow();
			waitForIdle();

			assertEquals(
					"The second back press should exit",
					Lifecycle.State.DESTROYED,
					awaitDestroyed(scenario));
		}
	}

	// ---------------------------------------------------------------------
	// The 300ms double-press guard, which only applies where the OS is not
	// providing predictive back animations
	// ---------------------------------------------------------------------

	/**
	 * Below API 36 the guard swallows a second back press arriving within
	 * 300ms of the first.
	 */
	@Test
	public void postListing_rapidDoubleBack_isGuardedBelowApi36() {

		assumeTrue(
				"The double-press guard only applies where predictive back is "
						+ "not in use",
				Build.VERSION.SDK_INT < 36);

		setBackAgain(true);

		try (ActivityScenario<PostListingActivity> scenario = ActivityScenario.launch(
				intentFor(mContext, PostListingActivity.class, TEST_SUBREDDIT_URL))) {

			waitForIdle();

			dispatchBack(scenario);
			waitForIdle();

			assertEquals(
					"The first back press should show the prompt",
					Lifecycle.State.RESUMED,
					scenario.getState());

			// Immediately again, inside the 300ms guard window
			dispatchBack(scenario);
			waitForIdle();

			assertEquals(
					"A back press within 300ms should be swallowed by the guard",
					Lifecycle.State.RESUMED,
					scenario.getState());

			// Once the guard window has passed, back should be honoured again
			SystemClock.sleep(500);

			dispatchBack(scenario);
			waitForIdle();

			assertEquals(
					"Back after the guard window should exit",
					Lifecycle.State.DESTROYED,
					awaitDestroyed(scenario));
		}
	}

	/**
	 * On API 36 the guard is deliberately skipped, as the predictive back
	 * animation already protects against accidental presses.
	 */
	@Test
	public void postListing_rapidDoubleBack_isNotGuardedOnApi36() {

		assumeTrue(Build.VERSION.SDK_INT >= 36);

		setBackAgain(true);

		try (ActivityScenario<PostListingActivity> scenario = ActivityScenario.launch(
				intentFor(mContext, PostListingActivity.class, TEST_SUBREDDIT_URL))) {

			waitForIdle();

			dispatchBack(scenario);
			waitForIdle();

			assertEquals(
					"The first back press should show the prompt",
					Lifecycle.State.RESUMED,
					scenario.getState());

			// Immediately again: this must NOT be swallowed
			dispatchBack(scenario);
			waitForIdle();

			assertEquals(
					"A rapid second back press should exit on Android 16",
					Lifecycle.State.DESTROYED,
					awaitDestroyed(scenario));
		}
	}

	/**
	 * The prompt expires after 5 seconds, after which back should prompt again
	 * rather than exiting.
	 */
	@Test
	public void postListing_backAgainEnabled_promptIsNotConsumedByRapidPresses() {

		setBackAgain(true);

		try (ActivityScenario<PostListingActivity> scenario = ActivityScenario.launch(
				intentFor(mContext, PostListingActivity.class, TEST_SUBREDDIT_URL))) {

			waitForIdle();

			pressBackUnconditionally();
			waitForIdle();

			assertEquals(Lifecycle.State.RESUMED, scenario.getState());

			// Let the 5 second window expire
			SystemClock.sleep(5500);

			pressBackUnconditionally();
			waitForIdle();

			assertEquals(
					"Back after the prompt expired should re-prompt, not exit",
					Lifecycle.State.RESUMED,
					scenario.getState());
		}
	}

	// ---------------------------------------------------------------------
	// HtmlViewActivity / WebViewActivity: WebView history
	// ---------------------------------------------------------------------

	private static final String HTML_NO_HISTORY = "<html><body>no history</body></html>";

	private static Intent htmlIntent(final Context context, final String html) {
		final Intent intent = new Intent(context, HtmlViewActivity.class);
		intent.putExtra("html", html);
		intent.putExtra("title", "test");
		return intent;
	}

	@Test
	public void htmlView_alwaysInterceptsBack() {

		try (ActivityScenario<HtmlViewActivity> scenario
					 = ActivityScenario.launch(htmlIntent(mContext, HTML_NO_HISTORY))) {

			waitForIdle();

			assertIntercepts(
					"The WebView activities must always intercept back, so they "
							+ "can navigate their history",
					true,
					scenario);
		}
	}

	/**
	 * With no history to go back through, {@code onBackButtonPressed()} returns
	 * false and the activity must still close. This exercises the re-dispatch
	 * path in BaseActivity (disable the callback, dispatch again, fall through
	 * to finishing the activity).
	 */
	@Test
	public void htmlView_noHistory_backExits() {

		try (ActivityScenario<HtmlViewActivity> scenario
					 = ActivityScenario.launch(htmlIntent(mContext, HTML_NO_HISTORY))) {

			waitForIdle();

			pressBackUnconditionally();
			waitForIdle();

			assertEquals(
					"Back with no WebView history should exit",
					Lifecycle.State.DESTROYED,
					awaitDestroyed(scenario));
		}
	}

	@Test
	public void htmlView_withHistory_backNavigatesHistoryThenExits() {

		try (ActivityScenario<HtmlViewActivity> scenario
					 = ActivityScenario.launch(htmlIntent(mContext, HTML_NO_HISTORY))) {

			waitForIdle();

			final AtomicReference<WebView> webViewRef = new AtomicReference<>();
			scenario.onActivity(activity -> webViewRef.set(findWebView(activity)));
			assertNotNull("Could not find the WebView", webViewRef.get());

			// Let the initial page settle, then navigate to a second page so
			// that the WebView has history to go back through
			SystemClock.sleep(1000);

			scenario.onActivity(activity -> findWebView(activity).loadDataWithBaseURL(
					"https://reddit.com/",
					"<html><body>second page</body></html>",
					"text/html; charset=utf-8",
					"UTF-8",
					null));

			assertNotNull(
					"WebView should have history to go back through",
					awaitCanGoBack(scenario));

			pressBackUnconditionally();
			waitForIdle();

			assertEquals(
					"Back should navigate the WebView, not close the activity",
					Lifecycle.State.RESUMED,
					scenario.getState());

			// The history entry should have been consumed
			final AtomicBoolean canStillGoBack = new AtomicBoolean(true);
			scenario.onActivity(activity -> canStillGoBack.set(
					findWebView(activity).canGoBack()));

			assertFalse(
					"The WebView should have navigated back",
					canStillGoBack.get());

			pressBackAfterGuardWindow();
			waitForIdle();

			assertEquals(
					"Once history is exhausted, back should exit",
					Lifecycle.State.DESTROYED,
					awaitDestroyed(scenario));
		}
	}

	/**
	 * Polls until the WebView reports that it has history, returning it (or
	 * null on timeout).
	 */
	private static WebView awaitCanGoBack(final ActivityScenario<?> scenario) {

		final AtomicReference<WebView> result = new AtomicReference<>();

		for (int attempt = 0; attempt < 100; attempt++) {

			scenario.onActivity(activity -> {
				final WebView webView = findWebView(activity);
				if (webView != null && webView.canGoBack()) {
					result.set(webView);
				}
			});

			if (result.get() != null) {
				return result.get();
			}

			SystemClock.sleep(100);
		}

		return null;
	}

	private static WebView findWebView(final android.app.Activity activity) {
		return findWebView(activity.findViewById(android.R.id.content));
	}

	private static WebView findWebView(final android.view.View view) {

		if (view instanceof WebView) {
			return (WebView)view;
		}

		if (view instanceof android.view.ViewGroup) {
			final android.view.ViewGroup group = (android.view.ViewGroup)view;
			for (int i = 0; i < group.getChildCount(); i++) {
				final WebView result = findWebView(group.getChildAt(i));
				if (result != null) {
					return result;
				}
			}
		}

		return null;
	}

	// ---------------------------------------------------------------------
	// MainActivity: two-pane navigation
	// ---------------------------------------------------------------------

	@Test
	public void mainActivity_singlePane_doesNotInterceptBack() {

		setTwoPane("never");

		try (ActivityScenario<MainActivity> scenario
					 = ActivityScenario.launch(MainActivity.class)) {

			waitForIdle();

			assertIntercepts(
					"MainActivity should not intercept back in single-pane mode",
					false,
					scenario);

			pressBackUnconditionally();
			waitForIdle();

			assertEquals(
					"Back should exit the app from the main menu",
					Lifecycle.State.DESTROYED,
					awaitDestroyed(scenario));
		}
	}

	@Test
	public void mainActivity_twoPane_menuShown_doesNotInterceptBack() {

		setTwoPane("force");

		try (ActivityScenario<MainActivity> scenario
					 = ActivityScenario.launch(MainActivity.class)) {

			waitForIdle();

			// The menu is showing, so there is nothing to restore: the system
			// should own the back gesture.
			assertIntercepts(
					"MainActivity should not intercept back while the main menu "
							+ "is showing",
					false,
					scenario);
		}
	}

	/**
	 * The main two-pane behaviour: once comments replace the main menu, back
	 * must restore the menu rather than exiting the app.
	 *
	 * <p>Requires network access, in the same way as the other UI tests in this
	 * package.
	 */
	@Test
	public void mainActivity_twoPane_backRestoresMenu() {

		setTwoPane("force");

		// So that tapping a post's title opens its comments. Tapping the post
		// body is not reliable, as the tap can land on an image preview, which
		// opens the link instead.
		setPostTapAction("title_comments");

		try (ActivityScenario<MainActivity> scenario
					 = ActivityScenario.launch(MainActivity.class)) {

			waitForIdle();

			// Main menu -> Front Page, which loads a post listing into the
			// other pane
			onView(allOf(
					withText(R.string.mainmenu_frontpage),
					isDisplayed()))
					.perform(click());

			// Wait for the post listing, then open a post's comments, which is
			// what replaces the main menu in two-pane mode
			awaitView(withId(R.id.reddit_post_title), 40);

			onView(firstMatching(allOf(
					withId(R.id.reddit_post_title),
					isDisplayed())))
					.perform(click());

			for (int attempt = 0; attempt < 40 && isMenuShown(scenario); attempt++) {
				SystemClock.sleep(500);
			}

			assertFalse(
					"The main menu should have been replaced by comments",
					isMenuShown(scenario));

			assertIntercepts(
					"MainActivity must intercept back once comments have "
							+ "replaced the main menu",
					true,
					scenario);

			pressBackUnconditionally();
			waitForIdle();
			onView(isRoot()).perform(UITestUtils.waitForSeconds(1));

			assertEquals(
					"Back should restore the main menu, not exit the app",
					Lifecycle.State.RESUMED,
					scenario.getState());

			assertTrue(
					"Back should have restored the main menu",
					isMenuShown(scenario));

			assertIntercepts(
					"Once the main menu is restored, back should no longer be "
							+ "intercepted",
					false,
					scenario);

			// And a further back press should now exit
			pressBackAfterGuardWindow();
			waitForIdle();

			assertEquals(
					"Back from the restored main menu should exit",
					Lifecycle.State.DESTROYED,
					awaitDestroyed(scenario));
		}
	}

	/**
	 * Matches only the first view satisfying the given matcher, so that a
	 * listing full of posts does not produce an ambiguous match.
	 */
	private static Matcher<View> firstMatching(final Matcher<View> matcher) {

		return new TypeSafeMatcher<View>() {

			private boolean mFound = false;

			@Override
			public void describeTo(final Description description) {
				description.appendText("first view matching: ");
				matcher.describeTo(description);
			}

			@Override
			protected boolean matchesSafely(final View view) {

				if (mFound || !matcher.matches(view)) {
					return false;
				}

				mFound = true;
				return true;
			}
		};
	}

	/**
	 * Reads MainActivity's private {@code isMenuShown} field, so that the test
	 * asserts on the activity's actual state rather than only on which views
	 * happen to be on screen.
	 */
	private static boolean isMenuShown(final ActivityScenario<MainActivity> scenario) {

		final AtomicBoolean result = new AtomicBoolean();

		scenario.onActivity(activity -> {
			try {
				final java.lang.reflect.Field field
						= MainActivity.class.getDeclaredField("isMenuShown");
				field.setAccessible(true);
				result.set(field.getBoolean(activity));
			} catch (final ReflectiveOperationException e) {
				throw new RuntimeException(e);
			}
		});

		return result.get();
	}

	// ---------------------------------------------------------------------
	// Progress dialogs
	// ---------------------------------------------------------------------

	/**
	 * The app's progress dialogs intercept back via
	 * {@code setOnKeyListener(KEYCODE_BACK)}, which predictive back no longer
	 * dispatches. They rely instead on their {@code OnCancelListener}, which
	 * the platform triggers via {@code Dialog.onBackPressed() -> cancel()}.
	 * This verifies that platform contract, which is what makes those dialogs
	 * keep working unchanged on Android 16.
	 */
	@Test
	public void progressDialog_backTriggersOnCancelListener() {

		try (ActivityScenario<HtmlViewActivity> scenario
					 = ActivityScenario.launch(htmlIntent(mContext, HTML_NO_HISTORY))) {

			waitForIdle();

			final AtomicBoolean cancelled = new AtomicBoolean(false);
			final AtomicReference<Dialog> dialogRef = new AtomicReference<>();

			scenario.onActivity(activity -> {
				@SuppressWarnings("deprecation")
				final android.app.ProgressDialog dialog
						= new android.app.ProgressDialog(activity);
				dialog.setTitle(R.string.comment_reply_submitting_title);
				dialog.setCancelable(true);
				dialog.setCanceledOnTouchOutside(false);
				dialog.setOnCancelListener(d -> cancelled.set(true));
				dialog.show();
				dialogRef.set(dialog);
			});

			waitForIdle();
			SystemClock.sleep(500);

			pressBackUnconditionally();
			waitForIdle();
			SystemClock.sleep(500);

			assertTrue(
					"Back should cancel the progress dialog (firing its "
							+ "OnCancelListener) on Android 16",
					cancelled.get());

			assertFalse(
					"The dialog should no longer be showing",
					dialogRef.get().isShowing());

			assertEquals(
					"Cancelling the dialog should not close the activity",
					Lifecycle.State.RESUMED,
					scenario.getState());
		}
	}
}
