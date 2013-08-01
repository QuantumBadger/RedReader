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
import org.holoeverywhere.app.Activity;
import org.holoeverywhere.preference.PreferenceManager;
import org.holoeverywhere.widget.FrameLayout;
import org.holoeverywhere.widget.LinearLayout;
import org.holoeverywhere.widget.TextView;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.reddit.RedditPreparedInboxItem;

public class RedditInboxItemView extends LinearLayout {

	private final TextView header;
	private final FrameLayout bodyHolder;

	private final int bodyCol;

	private final boolean showLinkButtons;

	private RedditPreparedInboxItem currentItem = null;

	public RedditInboxItemView(final Context context, final int headerCol, final int bodyCol) {

		super(context);
		this.bodyCol = bodyCol;

		setOrientation(HORIZONTAL);

		final LinearLayout main = new LinearLayout(context);
		main.setOrientation(VERTICAL);

		header = new TextView(context);
		header.setTextSize(11.0f);
		header.setTextColor(headerCol);
		main.addView(header);

		bodyHolder = new FrameLayout(context);
		bodyHolder.setPadding(0, General.dpToPixels(context, 2), 0, 0);
		main.addView(bodyHolder);

		final int paddingPixels = General.dpToPixels(context, 8.0f);
		setPadding(paddingPixels + paddingPixels, paddingPixels, paddingPixels, paddingPixels);

		setDescendantFocusability(FOCUS_BLOCK_DESCENDANTS);

		showLinkButtons = PrefsUtility.pref_appearance_linkbuttons(context, PreferenceManager.getDefaultSharedPreferences(context));

		addView(main);
	}

	public void reset(final Activity context, final RedditPreparedInboxItem item) {

		currentItem = item;

		header.setText(item.getHeader());

		bodyHolder.removeAllViews();
		bodyHolder.addView(item.getBody(context, 13.0f, bodyCol, showLinkButtons));
	}

	public void handleInboxClick(Activity activity) {
		if(currentItem != null) currentItem.handleInboxClick(activity);
	}
}
