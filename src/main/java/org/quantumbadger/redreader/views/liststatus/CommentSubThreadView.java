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

package org.quantumbadger.redreader.views.liststatus;

import android.content.res.TypedArray;
import androidx.appcompat.app.AppCompatActivity;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;

public final class CommentSubThreadView extends StatusListItemView {

	private final PostCommentListingURL mUrl;

	public CommentSubThreadView(
			final AppCompatActivity activity,
			final PostCommentListingURL url,
			int messageRes) {

		super(activity);

		mUrl = url;

		final TypedArray attr = activity.obtainStyledAttributes(new int[] {
				R.attr.rrCommentSpecificThreadHeaderBackCol,
				R.attr.rrCommentSpecificThreadHeaderTextCol
		});

		final int rrCommentSpecificThreadHeaderBackCol = attr.getColor(0, 0);
		final int rrCommentSpecificThreadHeaderTextCol = attr.getColor(1, 0);

		attr.recycle();

		final TextView textView = new TextView(activity);
		textView.setText(messageRes);
		textView.setTextColor(rrCommentSpecificThreadHeaderTextCol);
		textView.setTextSize(15.0f);
		textView.setPadding((int) (15 * dpScale), (int) (10 * dpScale), (int) (10 * dpScale), (int) (4 * dpScale));

		final TextView messageView = new TextView(activity);
		messageView.setText(R.string.comment_header_specific_thread_message);
		messageView.setTextColor(rrCommentSpecificThreadHeaderTextCol);
		messageView.setTextSize(12.0f);
		messageView.setPadding((int) (15 * dpScale), 0, (int) (10 * dpScale), (int) (10 * dpScale));

		final LinearLayout layout = new LinearLayout(activity);
		layout.setOrientation(LinearLayout.VERTICAL);
		layout.addView(textView);
		layout.addView(messageView);

		setContents(layout);
		setDescendantFocusability(FOCUS_BLOCK_DESCENDANTS);

		setBackgroundColor(rrCommentSpecificThreadHeaderBackCol);

		setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(final View v) {
				final PostCommentListingURL allComments = mUrl.commentId(null);
				LinkHandler.onLinkClicked(activity, allComments.toString());
			}
		});
	}

}
