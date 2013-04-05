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
import com.laurencedawson.activetextview.ActiveTextView;
import org.holoeverywhere.widget.FrameLayout;
import org.holoeverywhere.widget.LinearLayout;
import org.holoeverywhere.widget.TextView;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.fragments.CommentListingFragment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedComment;

public class RedditCommentView extends LinearLayout {

	private RedditPreparedComment comment;

	private final TextView header;
	private final FrameLayout bodyHolder;

	private final int bodyCol;

	public RedditCommentView(final Context context, final int headerCol, final int bodyCol) {

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

		setDescendantFocusability(FOCUS_BLOCK_DESCENDANTS);

		addView(main);
	}

	public void reset(final Context context, final CommentListingFragment fragment, final RedditPreparedComment comment, final ActiveTextView.OnLinkClickedListener listener) {

		this.comment = comment;

		final int paddingPixels = General.dpToPixels(context, 8.0f);
		final int paddingPixelsPerIndent = General.dpToPixels(context, 10.0f); // TODO Add in vertical lines

		setPadding(paddingPixels + paddingPixelsPerIndent * comment.indentation, paddingPixels, paddingPixels, paddingPixels);

		if(!comment.isCollapsed()) {
			header.setText(comment.header);
		} else {
			header.setText("[ + ]  " + comment.header);
		}

		bodyHolder.removeAllViews();
		bodyHolder.addView(comment.body.generate(context, 13.0f, bodyCol, new ActiveTextView.OnLinkClickedListener() {
			public void onClick(String url) {
				if(url != null) {
					listener.onClick(url);
				} else {
					fragment.handleCommentVisibilityToggle(RedditCommentView.this);
				}
			}
		}));

		updateVisibility();
	}

	private void updateVisibility() {

		if(comment.isCollapsed()) {

			bodyHolder.setVisibility(GONE);

			// TODO handle using strings
			if(comment.replyCount() == 1) {
				header.setText("[ + ]  " + comment.header + " (1 reply)"); // TODO string
			} else {
				header.setText("[ + ]  " + comment.header + " (" + comment.replyCount() + " replies)"); // TODO string
			}

		} else {
			bodyHolder.setVisibility(VISIBLE);
			header.setText(comment.header);
		}
	}

	public boolean handleVisibilityToggle() {
		comment.toggleVisibility();
		updateVisibility();
		return comment.isCollapsed();
	}

	public RedditPreparedComment getComment() {
		return comment;
	}

	public void updateAppearance() {
		header.setText(comment.header);
	}
}
