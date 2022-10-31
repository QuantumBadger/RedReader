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

package org.saiditnet.redreader.views;

import android.content.Context;
import android.content.Intent;
import android.content.res.TypedArray;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.activities.MoreCommentsListingActivity;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.reddit.RedditCommentListItem;
import org.saiditnet.redreader.reddit.url.PostCommentListingURL;
import org.saiditnet.redreader.reddit.url.RedditURLParser;

import java.util.ArrayList;

public class LoadMoreCommentsView extends LinearLayout {

	private final IndentView mIndentView;
	private final TextView mTitleView;
	private RedditCommentListItem mItem;
	private final RedditURLParser.RedditURL mCommentListingURL;

	public LoadMoreCommentsView(
			final Context context,
			final RedditURLParser.RedditURL commentListingURL) {

		super(context);

		mCommentListingURL = commentListingURL;

		setOrientation(VERTICAL);

		final View divider = new View(context);
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

		final ImageView icon;

		{
			final TypedArray appearance = context.obtainStyledAttributes(new int[]{
					R.attr.rrIconForward,
					R.attr.rrListItemBackgroundCol,
					R.attr.rrListDividerCol});

			icon = new ImageView(context);
			icon.setImageDrawable(appearance.getDrawable(0));

			layout.setBackgroundColor(appearance.getColor(1, General.COLOR_INVALID));

			divider.setBackgroundColor(appearance.getColor(2, General.COLOR_INVALID));

			appearance.recycle();
		}

		icon.setScaleX(0.75f);
		icon.setScaleY(0.75f);

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

				if(mCommentListingURL.pathType() == RedditURLParser.POST_COMMENT_LISTING_URL) {

					final PostCommentListingURL listingUrl = mCommentListingURL.asPostCommentListURL();

					final ArrayList<String> commentIds = new ArrayList<>(16);
					for(final PostCommentListingURL url : mItem.asLoadMore().getMoreUrls(mCommentListingURL)) {
						commentIds.add(url.commentId);
					}

					final Intent intent = new Intent(context, MoreCommentsListingActivity.class);
					intent.putExtra("postId", listingUrl.postId);
					intent.putStringArrayListExtra("commentIds", commentIds);
					context.startActivity(intent);

				} else {
					General.quickToast(context, R.string.load_more_comments_failed_unknown_url_type);
				}
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
