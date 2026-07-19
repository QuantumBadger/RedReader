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

import android.graphics.drawable.ColorDrawable;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.core.graphics.Insets;
import androidx.core.view.WindowInsetsCompat;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.quantumbadger.redreader.activities.ChangelogActivity;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.robolectric.Robolectric;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.android.controller.ActivityController;
import org.robolectric.annotation.Config;

@RunWith(RobolectricTestRunner.class)
@Config(sdk = 35, qualifiers = "w411dp-h891dp")
public class EdgeToEdgeInsetsTest {

	private static final int STATUS_BAR_HEIGHT = 100;
	private static final int NAV_BAR_HEIGHT = 150;

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

	@org.junit.Before
	public void initPrefs() {

		// Initialise PrefsUtility's static state directly: the full init()
		// path calls General.initAppConfig(), which doesn't work under
		// Robolectric
		final android.app.Application app
				= org.robolectric.RuntimeEnvironment.getApplication();

		try {
			final java.lang.reflect.Field resField
					= PrefsUtility.class.getDeclaredField("mRes");
			resField.setAccessible(true);
			resField.set(null, app.getResources());

			final java.lang.reflect.Field prefsField
					= PrefsUtility.class.getDeclaredField("sharedPrefs");
			prefsField.setAccessible(true);
			prefsField.set(null, org.quantumbadger.redreader.common.General
					.getSharedPrefs(app));

		} catch (final ReflectiveOperationException e) {
			throw new RuntimeException(e);
		}
	}

	@Test
	public void testNavBarIconAppearanceForWhitePref() {

		final android.app.Application app
				= org.robolectric.RuntimeEnvironment.getApplication();

		org.quantumbadger.redreader.common.General.getSharedPrefs(app)
				.edit()
				.putString("pref_appearance_navbar_color", "white")
				.apply();

		final android.content.Intent intent = new android.content.Intent(
				app, org.quantumbadger.redreader.activities.HtmlViewActivity.class);
		intent.putExtra("html", "<p>test</p>");
		intent.putExtra("title", "test");

		final org.quantumbadger.redreader.activities.HtmlViewActivity activity
				= Robolectric.buildActivity(
						org.quantumbadger.redreader.activities.HtmlViewActivity.class,
						intent)
				.setup()
				.get();

		final int appearance = activity.getWindow().getInsetsController()
				.getSystemBarsAppearance();

		final int appearanceForceLightNavigationBars = 1 << 9;

		System.out.println("==== Final appearance bits: 0x"
				+ Integer.toHexString(appearance) + " ====");

		Assert.assertEquals(
				"FORCE_LIGHT_NAVIGATION_BARS should be clear",
				0,
				appearance & appearanceForceLightNavigationBars);

		Assert.assertNotEquals(
				"LIGHT_NAVIGATION_BARS should be set for a white nav bar",
				0,
				appearance & android.view.WindowInsetsController
						.APPEARANCE_LIGHT_NAVIGATION_BARS);
	}

	@Test
	public void testSystemBarScrimsAppliedOnInsetDispatch() {

		final ActivityController<ChangelogActivity> controller
				= Robolectric.buildActivity(ChangelogActivity.class).setup();

		final ChangelogActivity activity = controller.get();

		final View decor = activity.getWindow().getDecorView();

		final WindowInsetsCompat insets = new WindowInsetsCompat.Builder()
				.setInsets(
						WindowInsetsCompat.Type.statusBars(),
						Insets.of(0, STATUS_BAR_HEIGHT, 0, 0))
				.setInsets(
						WindowInsetsCompat.Type.navigationBars(),
						Insets.of(0, 0, 0, NAV_BAR_HEIGHT))
				.setVisible(WindowInsetsCompat.Type.statusBars(), true)
				.setVisible(WindowInsetsCompat.Type.navigationBars(), true)
				.build();

		decor.dispatchApplyWindowInsets(insets.toWindowInsets());

		final int width = 1080;
		final int height = 2400;

		decor.measure(
				View.MeasureSpec.makeMeasureSpec(width, View.MeasureSpec.EXACTLY),
				View.MeasureSpec.makeMeasureSpec(height, View.MeasureSpec.EXACTLY));
		decor.layout(0, 0, width, height);

		System.out.println("==== Hierarchy after inset dispatch ====");
		System.out.println(describe(decor, 0));

		final ViewGroup content = decor.findViewById(android.R.id.content);
		Assert.assertNotNull(content);
		Assert.assertEquals(1, content.getChildCount());

		final ViewGroup root = (ViewGroup)content.getChildAt(0);

		Assert.assertTrue(
				"Expected wrapper FrameLayout with content + 4 scrims, got "
						+ root.getClass() + " with " + root.getChildCount() + " children",
				root instanceof FrameLayout && root.getChildCount() == 5);

		final View wrappedContent = root.getChildAt(0);
		final ViewGroup.MarginLayoutParams contentParams
				= (ViewGroup.MarginLayoutParams)wrappedContent.getLayoutParams();

		Assert.assertEquals(
				"Content top margin should equal status bar inset",
				STATUS_BAR_HEIGHT,
				contentParams.topMargin);

		Assert.assertEquals(
				"Content bottom margin should equal nav bar inset",
				NAV_BAR_HEIGHT,
				contentParams.bottomMargin);

		// Children 1-4: left, right, top, bottom scrims
		final View scrimTop = root.getChildAt(3);
		final View scrimBottom = root.getChildAt(4);

		Assert.assertEquals(
				"Top scrim height should equal status bar inset",
				STATUS_BAR_HEIGHT,
				scrimTop.getLayoutParams().height);

		Assert.assertEquals(
				"Bottom scrim height should equal nav bar inset",
				NAV_BAR_HEIGHT,
				scrimBottom.getLayoutParams().height);

		Assert.assertTrue(
				"Bottom scrim should have a solid colour background",
				scrimBottom.getBackground() instanceof ColorDrawable);

		System.out.println("Bottom scrim colour: #" + Integer.toHexString(
				((ColorDrawable)scrimBottom.getBackground()).getColor()));
	}
}
