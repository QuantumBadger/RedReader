package org.quantumbadger.redreader.activities;

import androidx.test.espresso.NoMatchingViewException;

import static androidx.test.espresso.Espresso.onView;
import static androidx.test.espresso.action.ViewActions.click;
import static androidx.test.espresso.matcher.ViewMatchers.withId;
import static androidx.test.espresso.matcher.ViewMatchers.withText;
import static org.hamcrest.Matchers.allOf;

public class UITestUtils {

	private UITestUtils() {}

	public static void handleFirstRunDialog() {

		try {
			onView(allOf(
					withId(android.R.id.button1),
					withText("Log in now")))
					.perform(click());

		} catch(final NoMatchingViewException e) {
			// Ignore, the first run dialog has already been shown
			return;
		}

		onView(allOf(
				withId(android.R.id.button3),
				withText("Close")))
				.perform(click());
	}
}
