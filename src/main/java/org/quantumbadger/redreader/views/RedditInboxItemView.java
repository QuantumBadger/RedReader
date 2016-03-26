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
import android.preference.PreferenceManager;
import android.support.v7.app.AppCompatActivity;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRThemeAttributes;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManagerVolatile;
import org.quantumbadger.redreader.reddit.prepared.RedditRenderableInboxItem;

public class RedditInboxItemView extends LinearLayout {

	private final TextView header;
	private final FrameLayout bodyHolder;

	private final RRThemeAttributes mTheme;

	private final boolean showLinkButtons;

	private RedditRenderableInboxItem currentItem = null;

	public RedditInboxItemView(final Context context, final RRThemeAttributes theme) {

		super(context);
		mTheme = theme;

		setOrientation(HORIZONTAL);

		final LinearLayout main = new LinearLayout(context);
		main.setOrientation(VERTICAL);

		header = new TextView(context);
		header.setTextSize(11.0f * theme.rrCommentFontScale);
		header.setTextColor(theme.rrCommentHeaderCol);
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

	public void reset(
			final AppCompatActivity context,
			final RedditChangeDataManagerVolatile changeDataManager,
			final RRThemeAttributes theme,
			final RedditRenderableInboxItem item) {

		currentItem = item;

		header.setText(item.getHeader(theme, changeDataManager, context));

		bodyHolder.removeAllViews();
		bodyHolder.addView(item.getBody(
				context,
				mTheme.rrCommentBodyCol,
				13.0f * mTheme.rrCommentFontScale,
				showLinkButtons));
	}

	public void handleInboxClick(AppCompatActivity activity) {
		if(currentItem != null) currentItem.handleInboxClick(activity);
	}

	public void handleInboxLongClick(AppCompatActivity activity) {
		if(currentItem != null) currentItem.handleInboxLongClick(activity);
	}
}
