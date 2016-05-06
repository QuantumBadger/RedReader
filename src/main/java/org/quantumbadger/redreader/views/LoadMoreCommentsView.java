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

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.MoreCommentsListingActivity;
import org.quantumbadger.redreader.common.AndroidApi;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.reddit.RedditCommentListItem;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;

import java.util.ArrayList;

public class LoadMoreCommentsView extends LinearLayout {

	private final IndentView mIndentView;
	private final TextView mTitleView;
	private RedditCommentListItem mItem;
	private final RedditURLParser.RedditURL mCommentListingURL;

	@SuppressLint("NewApi")
	public LoadMoreCommentsView(
			final Context context,
			final RedditURLParser.RedditURL commentListingURL) {

		super(context);

		mCommentListingURL = commentListingURL;

		// TODO setDescendantFocusability(FOCUS_BLOCK_DESCENDANTS);

		setOrientation(VERTICAL);

		final View divider = new View(context);
		divider.setBackgroundColor(Color.argb(128, 128, 128, 128)); // TODO better
		addView(divider);

		divider.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;
		divider.getLayoutParams().height = 1;

		final LinearLayout layout = new LinearLayout(context);
		layout.setOrientation(HORIZONTAL);
		addView(layout);
		final int marginPx = General.dpToPixels(context, 8);

		layout.setGravity(Gravity.CENTER_VERTICAL);

		mIndentView = new IndentView(context);
		layout.addView(mIndentView);
		mIndentView.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;

		final TypedArray appearance = context.obtainStyledAttributes(new int[]{R.attr.rrIconForward});
		final ImageView icon = new ImageView(context);
		icon.setImageDrawable(appearance.getDrawable(0));
		appearance.recycle();

		if(AndroidApi.isGreaterThanOrEqualTo(11)) {
			icon.setScaleX(0.75f);
			icon.setScaleY(0.75f);
		}
		layout.addView(icon);
		((LinearLayout.LayoutParams)icon.getLayoutParams()).setMargins(marginPx, marginPx, marginPx, marginPx);

		final LinearLayout textLayout = new LinearLayout(context);
		textLayout.setOrientation(VERTICAL);
		layout.addView(textLayout);
		((LinearLayout.LayoutParams)textLayout.getLayoutParams()).setMargins(0, marginPx, marginPx, marginPx);

		mTitleView = new TextView(context);
		mTitleView.setText("Error: text not set");
		mTitleView.setTextSize(13f);
		textLayout.addView(mTitleView);

		setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(final View v) {
				final ArrayList<String> urls = new ArrayList<>(16);
				for(final PostCommentListingURL url : mItem.asLoadMore().getMoreUrls(mCommentListingURL)) {
					urls.add(url.toString());
				}

				final Intent intent = new Intent(context, MoreCommentsListingActivity.class);
				intent.putStringArrayListExtra("urls", urls);
				context.startActivity(intent);
			}
		});
	}

	public void reset(final RedditCommentListItem item) {

		mItem = item;

		final StringBuilder title = new StringBuilder(getContext().getString(R.string.more_comments_button_text));
		final int count = item.asLoadMore().getCount();

		title.append(getResources().getQuantityString(R.plurals.subtitle_replies, count, count));

		mTitleView.setText(title);
		mIndentView.setIndentation(item.getIndent());
	}

}
