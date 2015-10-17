/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package com.konneh.scroll.views;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.TypedArray;
import android.view.Gravity;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import com.konneh.scroll.R;
import com.konneh.scroll.common.AndroidApi;
import com.konneh.scroll.common.General;
import com.konneh.scroll.reddit.RedditCommentListItem;
import com.konneh.scroll.reddit.url.PostCommentListingURL;

import java.util.List;

public class LoadMoreCommentsView extends FrameLayout {

	private final IndentView mIndentView;
	private final TextView mTitleView;
	private RedditCommentListItem mItem;

	@SuppressLint("NewApi")
	public LoadMoreCommentsView(Context context) {

		super(context);

		setDescendantFocusability(FOCUS_BLOCK_DESCENDANTS);

		final LinearLayout layout = new LinearLayout(context);
		layout.setOrientation(LinearLayout.HORIZONTAL);
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
		textLayout.setOrientation(LinearLayout.VERTICAL);
		layout.addView(textLayout);
		((LinearLayout.LayoutParams)textLayout.getLayoutParams()).setMargins(0, marginPx, marginPx, marginPx);

		mTitleView = new TextView(context);
		mTitleView.setText("Error: text not set");
		mTitleView.setTextSize(13f);
		textLayout.addView(mTitleView);
	}

	public void reset(final RedditCommentListItem item) {

		mItem = item;

		final StringBuilder title = new StringBuilder(getContext().getString(R.string.more_comments_button_text));
		final int count = item.asLoadMore().getCount();

		if(count == 1) {
			title
					.append(" (1 ")
					.append(getContext().getString(R.string.subtitle_reply))
					.append(")");

		} else if(count > 1) {
			title
					.append(" (")
					.append(count).append(" ")
					.append(getContext().getString(R.string.subtitle_replies))
					.append(")");

		} else {
			title.append("...");
		}

		mTitleView.setText(title);
		mIndentView.setIndentation(item.getIndent());
	}

	public List<PostCommentListingURL> getUrls() {
		return mItem.asLoadMore().getMoreUrls();
	}
}
