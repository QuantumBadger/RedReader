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

package org.quantumbadger.redreader.views;

import android.content.Context;
import android.graphics.Color;
import android.graphics.Typeface;
import org.apache.commons.lang3.StringEscapeUtils;
import org.holoeverywhere.widget.LinearLayout;
import org.holoeverywhere.widget.TextView;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.views.list.RRTouchable;

import java.text.NumberFormat;
import java.util.Locale;

public final class SubredditHeader extends LinearLayout implements RRTouchable {

	// TODO make XML
	public SubredditHeader(final Context context, final RedditSubreddit subreddit) {

		super(context);

		final float dpScale = context.getResources().getDisplayMetrics().density;

		setOrientation(LinearLayout.VERTICAL);

		final int sidesPadding = (int)(15.0f * dpScale);
		final int topPadding = (int)(10.0f * dpScale);

		setPadding(sidesPadding, topPadding, sidesPadding, topPadding);

		final Typeface tf = Typeface.createFromAsset(context.getAssets(), "fonts/Roboto-Light.ttf");

		final TextView title = new TextView(context);
		title.setTextSize(22.0f);
		title.setTypeface(tf);
		title.setText(StringEscapeUtils.unescapeHtml4(subreddit.title));
		title.setTextColor(Color.WHITE);
		addView(title);

		final TextView subs = new TextView(context);
		subs.setTextSize(14.0f);

		if(subreddit.subscribers == null) {
			subs.setText("Subscriber count is unknown");
		} else {
			subs.setText(NumberFormat.getNumberInstance(Locale.getDefault()).format(subreddit.subscribers) + " subscribers");
		}

		subs.setTextColor(Color.rgb(200, 200, 200));
		addView(subs);

		setBackgroundColor(Color.rgb(50, 50, 50)); // TODO theme color
	}

	public void rrOnClick(final int x, final int y) {}
	public void rrOnLongClick() {}
	public void rrOnFingerDown() {}
	public void rrOnSwipeDelta(final float dx) {}
	public void rrOnFingerUp() {}

	public boolean rrAllowLongClick() {
		return false;
	}
}
