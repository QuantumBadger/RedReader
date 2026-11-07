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

package org.quantumbadger.redreader.test.general;

import android.app.Activity;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.os.Looper;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.graphics.Insets;
import androidx.core.view.WindowInsetsCompat;
import androidx.recyclerview.widget.RecyclerView;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.ChangelogActivity;
import org.quantumbadger.redreader.activities.HtmlViewActivity;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.settings.SettingsActivity;
import org.robolectric.Robolectric;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.Shadows;
import org.robolectric.annotation.Config;

@RunWith(RobolectricTestRunner.class)
@Config(sdk = 35, qualifiers = "w411dp-h891dp")
public class EdgeToEdgeInsetsTest {

	private static final int STATUS_BAR_HEIGHT = 100;
	private static final int NAV_BAR_HEIGHT = 150;
	private static final int GESTURE_HANDLE_HEIGHT = 60;

	private static final int WIDTH = 1080;
	private static final int HEIGHT = 2400;

	private static final int APPEARANCE_FORCE_LIGHT_NAVIGATION_BARS = 1 << 9;

	private static String describe(final View view, final int depth) {

		final StringBuilder sb = new StringBuilder();

		for (int i = 0; i < depth; i++) {
			sb.append("  ");
		}

		sb.append(view.getClass().getSimpleName());
		sb.append(" bounds=(").append(view.getLeft()).append(",").append(view.getTop())
				.append(",").append(view.getRight()).append(",").append(view.getBottom())
				.append(")");

		if (view.getLayoutParams() instanceof ViewGroup.MarginLayoutParams) {
			final ViewGroup.MarginLayoutParams lp
					= (ViewGroup.MarginLayoutParams)view.getLayoutParams();
			sb.append(" margins=(").append(lp.leftMargin).append(",").append(lp.topMargin)
					.append(",").append(lp.rightMargin).append(",").append(lp.bottomMargin)
					.append(")");
		}

		if (view.getBackground() instanceof ColorDrawable) {
			sb.append(" bg=#").append(Integer.toHexString(
					((ColorDrawable)view.getBackground()).getColor()));
		}

		sb.append(" fitsSystemWindows=").append(view.getFitsSystemWindows());
		sb.append(" padding=(").append(view.getPaddingLeft()).append(",")
				.append(view.getPaddingTop()).append(",").append(view.getPaddingRight())
				.append(",").append(view.getPaddingBottom()).append(")");

		sb.append("\n");

		if (view instanceof ViewGroup) {
			final ViewGroup group = (ViewGroup)view;
			for (int i = 0; i < group.getChildCount(); i++) {
				sb.append(describe(group.getChildAt(i), depth + 1));
			}
		}

		return sb.toString();
	}

	@Nullable
	private static <T extends View> T findFirst(
			@NonNull final View view,
			@NonNull final Class<T> type) {

		if (type.isInstance(view)) {
			return type.cast(view);
		}

		if (view instanceof ViewGroup) {
			final ViewGroup group = (ViewGroup)view;
			for (int i = 0; i < group.getChildCount(); i++) {
				final T result = findFirst(group.getChildAt(i), type);
				if (result != null) {
					return result;
				}
			}
		}

		return null;
	}

	private static android.app.Application app() {
		return org.robolectric.RuntimeEnvironment.getApplication();
	}

	@org.junit.Before
	public void initPrefs() {

		// Initialise PrefsUtility's static state directly: the full init()
		// path calls General.initAppConfig(), which doesn't work under
		// Robolectric
		try {
			// General caches the preferences statically, so without this the
			// preferences set by one test would leak into the next
			final java.lang.reflect.Field cachedPrefsField
					= General.class.getDeclaredField("mPrefs");
			cachedPrefsField.setAccessible(true);
			((java.util.concurrent.atomic.AtomicReference<?>)cachedPrefsField.get(null))
					.set(null);

			final java.lang.reflect.Field resField
					= PrefsUtility.class.getDeclaredField("mRes");
			resField.setAccessible(true);
			resField.set(null, app().getResources());

			final java.lang.reflect.Field prefsField
					= PrefsUtility.class.getDeclaredField("sharedPrefs");
			prefsField.setAccessible(true);
			prefsField.set(null, General.getSharedPrefs(app()));

			final java.lang.reflect.Field contextField
					= PrefsUtility.class.getDeclaredField("appContext");
			contextField.setAccessible(true);
			contextField.set(null, app());

		} catch (final ReflectiveOperationException e) {
			throw new RuntimeException(e);
		}
	}

	private static int navBarAppearance(@NonNull final Activity activity) {
		return activity.getWindow().getInsetsController().getSystemBarsAppearance();
	}

	@NonNull
	private static HtmlViewActivity setUpHtmlViewActivity() {

		final android.content.Intent intent
				= new android.content.Intent(app(), HtmlViewActivity.class);
		intent.putExtra("html", "<p>test</p>");
		intent.putExtra("title", "test");

		return Robolectric.buildActivity(HtmlViewActivity.class, intent).setup().get();
	}

	@Test
	public void testNavBarIconAppearanceForLightTheme() {

		// The default theme is light, and no bar has been drawn over a scrim
		// yet, so the icons should contrast with the theme
		final int appearance = navBarAppearance(setUpHtmlViewActivity());

		Assert.assertEquals(
				"FORCE_LIGHT_NAVIGATION_BARS should be clear",
				0,
				appearance & APPEARANCE_FORCE_LIGHT_NAVIGATION_BARS);

		Assert.assertNotEquals(
				"LIGHT_NAVIGATION_BARS should be set for a light theme",
				0,
				appearance & android.view.WindowInsetsController
						.APPEARANCE_LIGHT_NAVIGATION_BARS);
	}

	@Test
	public void testNavBarIconAppearanceForDarkTheme() {

		General.getSharedPrefs(app())
				.edit()
				.putString("pref_appearance_theme", "night")
				.apply();

		final int appearance = navBarAppearance(setUpHtmlViewActivity());

		Assert.assertEquals(
				"FORCE_LIGHT_NAVIGATION_BARS should be clear",
				0,
				appearance & APPEARANCE_FORCE_LIGHT_NAVIGATION_BARS);

		Assert.assertEquals(
				"LIGHT_NAVIGATION_BARS should be clear for a dark theme",
				0,
				appearance & android.view.WindowInsetsController
						.APPEARANCE_LIGHT_NAVIGATION_BARS);
	}

	/**
	 * Dispatches insets for a status bar plus either a 3-button navigation
	 * bar (tappable along its whole height) or a gesture navigation handle
	 * (no tappable area), then lays the decor out.
	 */
	private static void dispatchInsets(
			@NonNull final Activity activity,
			final int navBarBottom,
			final boolean threeButton) {

		dispatchInsets(activity, navBarBottom, threeButton, true);
	}

	private static void dispatchInsets(
			@NonNull final Activity activity,
			final int navBarBottom,
			final boolean threeButton,
			final boolean statusBarVisible) {

		final View decor = activity.getWindow().getDecorView();

		final WindowInsetsCompat insets = new WindowInsetsCompat.Builder()
				.setInsets(
						WindowInsetsCompat.Type.statusBars(),
						Insets.of(0, statusBarVisible ? STATUS_BAR_HEIGHT : 0, 0, 0))
				.setInsets(
						WindowInsetsCompat.Type.navigationBars(),
						Insets.of(0, 0, 0, navBarBottom))
				.setInsets(
						WindowInsetsCompat.Type.tappableElement(),
						Insets.of(0, 0, 0, threeButton ? navBarBottom : 0))
				.setVisible(WindowInsetsCompat.Type.statusBars(), statusBarVisible)
				.setVisible(WindowInsetsCompat.Type.navigationBars(), true)
				.build();

		decor.dispatchApplyWindowInsets(insets.toWindowInsets());

		decor.measure(
				View.MeasureSpec.makeMeasureSpec(WIDTH, View.MeasureSpec.EXACTLY),
				View.MeasureSpec.makeMeasureSpec(HEIGHT, View.MeasureSpec.EXACTLY));
		decor.layout(0, 0, WIDTH, HEIGHT);

		System.out.println("==== Hierarchy after inset dispatch ====");
		System.out.println(describe(decor, 0));
	}

	@NonNull
	private static ViewGroup getScrimRoot(@NonNull final Activity activity) {

		final ViewGroup content
				= activity.getWindow().getDecorView().findViewById(android.R.id.content);
		Assert.assertNotNull(content);
		Assert.assertEquals(1, content.getChildCount());

		final ViewGroup root = (ViewGroup)content.getChildAt(0);

		Assert.assertTrue(
				"Expected wrapper FrameLayout with content + 4 scrims, got "
						+ root.getClass() + " with " + root.getChildCount() + " children",
				root instanceof FrameLayout && root.getChildCount() == 5);

		return root;
	}

	private static int contentBottomMargin(@NonNull final ViewGroup root) {
		return ((ViewGroup.MarginLayoutParams)root.getChildAt(0).getLayoutParams())
				.bottomMargin;
	}

	// Children 1-4: left, right, top, bottom scrims
	@NonNull
	private static View topScrim(@NonNull final ViewGroup root) {
		return root.getChildAt(3);
	}

	@NonNull
	private static View bottomScrim(@NonNull final ViewGroup root) {
		return root.getChildAt(4);
	}

	@Test
	public void testThreeButtonNavigation() {

		// ChangelogActivity doesn't lay its content out behind the nav bar
		final ChangelogActivity activity
				= Robolectric.buildActivity(ChangelogActivity.class).setup().get();

		dispatchInsets(activity, NAV_BAR_HEIGHT, true);

		final ViewGroup root = getScrimRoot(activity);

		Assert.assertEquals(
				"Content top margin should equal status bar inset",
				STATUS_BAR_HEIGHT,
				((ViewGroup.MarginLayoutParams)root.getChildAt(0).getLayoutParams())
						.topMargin);

		Assert.assertEquals(
				"Content bottom margin should equal nav bar inset",
				NAV_BAR_HEIGHT,
				contentBottomMargin(root));

		Assert.assertEquals(
				"Top scrim height should equal status bar inset",
				STATUS_BAR_HEIGHT,
				topScrim(root).getLayoutParams().height);

		Assert.assertEquals(
				"Bottom scrim height should equal nav bar inset",
				NAV_BAR_HEIGHT,
				bottomScrim(root).getLayoutParams().height);

		Assert.assertTrue(
				"Bottom scrim should have a colour background",
				bottomScrim(root).getBackground() instanceof ColorDrawable);

		final int scrimColour
				= ((ColorDrawable)bottomScrim(root).getBackground()).getColor();

		System.out.println("Bottom scrim colour: #" + Integer.toHexString(scrimColour));

		// ChangelogActivity's nav bar colour is #555555, which is dark
		Assert.assertEquals(
				"Bottom scrim should be tinted with the activity's nav bar colour",
				Color.rgb(0x55, 0x55, 0x55),
				scrimColour | 0xFF000000);

		final int alpha = Color.alpha(scrimColour);

		Assert.assertTrue(
				"Bottom scrim should be translucent, got alpha " + alpha,
				alpha > 0 && alpha < 0xFF);

		Assert.assertEquals(
				"LIGHT_NAVIGATION_BARS should be clear over a dark scrim",
				0,
				navBarAppearance(activity) & android.view.WindowInsetsController
						.APPEARANCE_LIGHT_NAVIGATION_BARS);
	}

	@Test
	public void testGestureNavigation() {

		// ChangelogActivity doesn't lay its content out behind the nav bar,
		// so the content should be inset above the gesture handle, but with
		// nothing drawn behind the handle
		final ChangelogActivity activity
				= Robolectric.buildActivity(ChangelogActivity.class).setup().get();

		dispatchInsets(activity, GESTURE_HANDLE_HEIGHT, false);

		final ViewGroup root = getScrimRoot(activity);

		Assert.assertEquals(
				"Content bottom margin should equal gesture handle inset",
				GESTURE_HANDLE_HEIGHT,
				contentBottomMargin(root));

		Assert.assertEquals(
				"Top scrim height should equal status bar inset",
				STATUS_BAR_HEIGHT,
				topScrim(root).getLayoutParams().height);

		Assert.assertEquals(
				"Nothing should be drawn behind the gesture handle",
				0,
				bottomScrim(root).getLayoutParams().height);
	}

	@Test
	public void testGestureNavigationWithContentBehindNavBar() {

		// SettingsActivity lays its preference list out behind the nav bar,
		// so the list itself should be padded rather than the content inset
		final SettingsActivity activity
				= Robolectric.buildActivity(SettingsActivity.class).setup().get();

		Shadows.shadowOf(Looper.getMainLooper()).idle();

		dispatchInsets(activity, GESTURE_HANDLE_HEIGHT, false);

		final ViewGroup root = getScrimRoot(activity);

		Assert.assertEquals(
				"Content should extend behind the gesture handle",
				0,
				contentBottomMargin(root));

		Assert.assertEquals(
				"Nothing should be drawn behind the gesture handle",
				0,
				bottomScrim(root).getLayoutParams().height);

		final RecyclerView list = findFirst(root, RecyclerView.class);
		Assert.assertNotNull("Preference list not found", list);

		Assert.assertEquals(
				"Preference list should be padded by the gesture handle inset",
				GESTURE_HANDLE_HEIGHT,
				list.getPaddingBottom());

		Assert.assertFalse(
				"Preference list should draw behind the gesture handle",
				list.getClipToPadding());
	}

	@Test
	public void testThreeButtonNavigationWithContentBehindNavBar() {

		final SettingsActivity activity
				= Robolectric.buildActivity(SettingsActivity.class).setup().get();

		Shadows.shadowOf(Looper.getMainLooper()).idle();

		dispatchInsets(activity, NAV_BAR_HEIGHT, true);

		final ViewGroup root = getScrimRoot(activity);

		Assert.assertEquals(
				"Content should extend behind the nav bar",
				0,
				contentBottomMargin(root));

		Assert.assertEquals(
				"Bottom scrim height should equal nav bar inset",
				NAV_BAR_HEIGHT,
				bottomScrim(root).getLayoutParams().height);

		final RecyclerView list = findFirst(root, RecyclerView.class);
		Assert.assertNotNull("Preference list not found", list);

		Assert.assertEquals(
				"Preference list should be padded by the nav bar inset",
				NAV_BAR_HEIGHT,
				list.getPaddingBottom());
	}

	@Test
	public void testBottomToolbar() {

		General.getSharedPrefs(app())
				.edit()
				.putBoolean("pref_appearance_bottom_toolbar_key", true)
				.apply();

		final ChangelogActivity activity
				= Robolectric.buildActivity(ChangelogActivity.class).setup().get();

		final View toolbar = activity.findViewById(R.id.rr_actionbar_toolbar);
		Assert.assertNotNull(toolbar);

		final int toolbarBaseHeight = toolbar.getLayoutParams().height;
		Assert.assertTrue("Expected a fixed toolbar height", toolbarBaseHeight > 0);

		dispatchInsets(activity, NAV_BAR_HEIGHT, true);

		final ViewGroup root = getScrimRoot(activity);

		Assert.assertEquals(
				"The toolbar takes the nav bar inset, not the content",
				0,
				contentBottomMargin(root));

		Assert.assertEquals(
				"Toolbar should be padded by the nav bar inset",
				NAV_BAR_HEIGHT,
				toolbar.getPaddingBottom());

		Assert.assertEquals(
				"Toolbar should grow by the nav bar inset",
				toolbarBaseHeight + NAV_BAR_HEIGHT,
				toolbar.getLayoutParams().height);

		Assert.assertEquals(
				"Bottom scrim height should equal nav bar inset",
				NAV_BAR_HEIGHT,
				bottomScrim(root).getLayoutParams().height);
	}

	/**
	 * A media-viewer-style activity, laying its content out behind both bars
	 */
	public static class ContentBehindBothBarsActivity extends ChangelogActivity {

		@Override
		protected boolean baseActivityContentExtendsBehindNavigationBar() {
			return true;
		}

		@Override
		protected boolean baseActivityContentExtendsBehindStatusBar() {
			return true;
		}
	}

	@Test
	public void testContentBehindStatusBar() {

		final ContentBehindBothBarsActivity activity
				= Robolectric.buildActivity(ContentBehindBothBarsActivity.class)
						.setup()
						.get();

		dispatchInsets(activity, NAV_BAR_HEIGHT, true);

		final ViewGroup root = getScrimRoot(activity);

		final ViewGroup.MarginLayoutParams contentParams
				= (ViewGroup.MarginLayoutParams)root.getChildAt(0).getLayoutParams();

		Assert.assertEquals("Content should extend behind the status bar", 0, contentParams.topMargin);
		Assert.assertEquals("Content should extend behind the nav bar", 0, contentParams.bottomMargin);

		Assert.assertEquals(
				"Top scrim height should equal status bar inset",
				STATUS_BAR_HEIGHT,
				topScrim(root).getLayoutParams().height);

		final int topScrimColour = ((ColorDrawable)topScrim(root).getBackground()).getColor();

		Assert.assertEquals(
				"Top scrim should be black",
				Color.BLACK,
				topScrimColour | 0xFF000000);

		final int bottomScrimColour
				= ((ColorDrawable)bottomScrim(root).getBackground()).getColor();

		Assert.assertEquals(
				"Top scrim should be as translucent as the dark nav bar scrim",
				Color.alpha(bottomScrimColour),
				Color.alpha(topScrimColour));

		Assert.assertTrue(
				"Top scrim should be translucent",
				Color.alpha(topScrimColour) > 0 && Color.alpha(topScrimColour) < 0xFF);
	}

	@Test
	public void testContentBehindHiddenStatusBar() {

		final ContentBehindBothBarsActivity activity
				= Robolectric.buildActivity(ContentBehindBothBarsActivity.class)
						.setup()
						.get();

		dispatchInsets(activity, NAV_BAR_HEIGHT, true, false);

		final ViewGroup root = getScrimRoot(activity);

		Assert.assertEquals(
				"Content should extend to the top of the screen",
				0,
				((ViewGroup.MarginLayoutParams)root.getChildAt(0).getLayoutParams())
						.topMargin);

		Assert.assertEquals(
				"Nothing should be drawn behind a hidden status bar",
				0,
				topScrim(root).getLayoutParams().height);
	}
}
