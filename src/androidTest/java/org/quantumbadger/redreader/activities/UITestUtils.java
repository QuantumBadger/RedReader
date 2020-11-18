package org.quantumbadger.redreader.activities;

import android.view.View;
import androidx.test.espresso.NoMatchingViewException;
import androidx.test.espresso.UiController;
import androidx.test.espresso.ViewAction;
import org.hamcrest.Matcher;
import org.hamcrest.core.IsAnything;

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

	public static ViewAction waitForSeconds(final long seconds) {
		return new ViewAction() {
			@Override
			public Matcher<View> getConstraints() {
				return new IsAnything<>();
			}

			@Override
			public String getDescription() {
				return "Wait for " + seconds + " seconds.";
			}

			@Override
			public void perform(final UiController uiController, final View view) {
				uiController.loopMainThreadForAtLeast(seconds * 1000);
			}
		};
	}
}
