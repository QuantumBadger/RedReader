package org.quantumbadger.redreader.activities;


import android.view.View;
import android.view.ViewGroup;
import android.view.ViewParent;
import androidx.test.espresso.ViewInteraction;
import androidx.test.filters.LargeTest;
import androidx.test.rule.ActivityTestRule;
import androidx.test.runner.AndroidJUnit4;
import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.hamcrest.TypeSafeMatcher;
import org.hamcrest.core.IsInstanceOf;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.quantumbadger.redreader.R;

import static androidx.test.espresso.Espresso.onView;
import static androidx.test.espresso.action.ViewActions.click;
import static androidx.test.espresso.action.ViewActions.typeTextIntoFocusedView;
import static androidx.test.espresso.assertion.ViewAssertions.matches;
import static androidx.test.espresso.contrib.RecyclerViewActions.actionOnItemAtPosition;
import static androidx.test.espresso.matcher.ViewMatchers.isDisplayed;
import static androidx.test.espresso.matcher.ViewMatchers.isFocused;
import static androidx.test.espresso.matcher.ViewMatchers.withChild;
import static androidx.test.espresso.matcher.ViewMatchers.withContentDescription;
import static androidx.test.espresso.matcher.ViewMatchers.withId;
import static androidx.test.espresso.matcher.ViewMatchers.withText;
import static org.hamcrest.Matchers.allOf;

@LargeTest
@RunWith(AndroidJUnit4.class)
public class UITestReadSelfPost {

	@Rule
	public ActivityTestRule<MainActivity> mActivityTestRule = new ActivityTestRule<>(MainActivity.class);

	@Test
	public void uITestReadSelfPost() {

		UITestUtils.handleFirstRunDialog();

		final ViewInteraction mainMenu = onView(
				allOf(withId(R.id.scrollbar_recyclerview_recyclerview),
						childAtPosition(
								withId(R.id.scrollbar_recyclerview_refreshlayout),
								0)));
		mainMenu.perform(actionOnItemAtPosition(2, click()));

		final ViewInteraction customSubredditTextBox
				= onView(allOf(withId(R.id.dialog_mainmenu_custom_value), isDisplayed()));

		customSubredditTextBox.perform(click());

		onView(allOf(
				withId(R.id.dialog_mainmenu_custom_value),
				isDisplayed(),
				isFocused())).perform(typeTextIntoFocusedView("redreader_public_test"));

		onView(allOf(withId(android.R.id.button1), withText("Go")))
				.perform(click());

		final ViewInteraction selfTextPost = onView(allOf(
				withId(R.id.reddit_post_layout_outer),
				withChild(allOf(
						withId(R.id.reddit_post_textLayout),
						withChild(allOf(
								withId(R.id.reddit_post_title),
								withText("Self text test post"))))),
				isDisplayed()));

		selfTextPost.perform(click());

		final ViewInteraction textView = onView(
				allOf(withText("This is the self text"),
						childAtPosition(
								childAtPosition(
										IsInstanceOf.instanceOf(android.widget.FrameLayout.class),
										0),
								0),
						isDisplayed()));
		textView.check(matches(isDisplayed()));

		final ViewInteraction textView2 = onView(
				allOf(withId(R.id.empty_view_text), withText("No comments yet."),
						childAtPosition(
								childAtPosition(
										IsInstanceOf.instanceOf(android.widget.FrameLayout.class),
										0),
								0),
						isDisplayed()));
		textView2.check(matches(isDisplayed()));

		final ViewInteraction imageButton = onView(
				allOf(withContentDescription("Previous Parent Comment"),
						childAtPosition(
								childAtPosition(
										IsInstanceOf.instanceOf(android.widget.FrameLayout.class),
										0),
								0),
						isDisplayed()));
		imageButton.check(matches(isDisplayed()));

		final ViewInteraction imageButton2 = onView(
				allOf(withContentDescription("Next Parent Comment"),
						childAtPosition(
								childAtPosition(
										IsInstanceOf.instanceOf(android.widget.FrameLayout.class),
										0),
								1),
						isDisplayed()));
		imageButton2.check(matches(isDisplayed()));

		final ViewInteraction textView3 = onView(
				allOf(withContentDescription("Sort Comments"),
						childAtPosition(
								childAtPosition(
										withId(R.id.rr_actionbar_toolbar),
										1),
								0),
						isDisplayed()));
		textView3.check(matches(isDisplayed()));

		final ViewInteraction textView4 = onView(
				allOf(withContentDescription("Refresh Comments"),
						childAtPosition(
								childAtPosition(
										withId(R.id.rr_actionbar_toolbar),
										1),
								1),
						isDisplayed()));
		textView4.check(matches(isDisplayed()));

		final ViewInteraction imageView = onView(
				allOf(withId(R.id.actionbar_title_back_image),
						childAtPosition(
								allOf(withId(R.id.actionbar_title_outer),
										childAtPosition(
												withId(R.id.rr_actionbar_toolbar),
												0)),
								0),
						isDisplayed()));
		imageView.check(matches(isDisplayed()));

		final ViewInteraction textView5 = onView(
				allOf(withId(R.id.actionbar_title_text), withText("Self text test post"),
						childAtPosition(
								allOf(withId(R.id.actionbar_title_outer),
										childAtPosition(
												withId(R.id.rr_actionbar_toolbar),
												0)),
								1),
						isDisplayed()));
		textView5.check(matches(isDisplayed()));
	}

	private static Matcher<View> childAtPosition(
			final Matcher<View> parentMatcher, final int position) {

		return new TypeSafeMatcher<View>() {
			@Override
			public void describeTo(final Description description) {
				description.appendText("Child at position " + position + " in parent ");
				parentMatcher.describeTo(description);
			}

			@Override
			public boolean matchesSafely(final View view) {
				final ViewParent parent = view.getParent();
				return parent instanceof ViewGroup && parentMatcher.matches(parent)
						&& view.equals(((ViewGroup)parent).getChildAt(position));
			}
		};
	}
}
