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

package org.quantumbadger.redreader.fragments;

import android.content.Context;
import android.os.Bundle;
import org.holoeverywhere.widget.LinearLayout;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.RRTime;
import org.quantumbadger.redreader.reddit.things.RedditComment;

public final class CommentPropertiesDialog extends PropertiesDialog {

	public static CommentPropertiesDialog newInstance(final RedditComment comment) {

		final CommentPropertiesDialog pp = new CommentPropertiesDialog();

		final Bundle args = new Bundle();
		args.putParcelable("comment", comment);
		pp.setArguments(args);

		return pp;
	}

	@Override
	protected String getTitle(Context context) {
		return context.getString(R.string.props_comment_title);
	}

	@Override
	protected void prepare(Context context, LinearLayout items) {

		final RedditComment comment = getArguments().getParcelable("comment");

		items.addView(propView(context, R.string.props_author, comment.author, false));

		if(comment.author_flair_text != null && comment.author_flair_text.length() > 0) {
			items.addView(propView(context, R.string.props_author_flair, comment.author_flair_text, false));
		}

		items.addView(propView(context, R.string.props_created, RRTime.formatDateTime(comment.created_utc * 1000, context), false));

		if(comment.edited instanceof Long) {
			items.addView(propView(context, R.string.props_edited, RRTime.formatDateTime((Long) comment.edited * 1000, context), false));
		} else {
			items.addView(propView(context, R.string.props_edited, R.string.props_never, false));
		}

		items.addView(propView(context, R.string.props_score,
				String.format("%d (%d %s, %d %s)",
						comment.ups - comment.downs,
						comment.ups, context.getString(R.string.props_up),
						comment.downs, context.getString(R.string.props_down)), false));

		items.addView(propView(context, R.string.props_subreddit, comment.subreddit, false));

		if(comment.body != null && comment.body.length() > 0) {
			items.addView(propView(context, R.string.props_body_markdown, comment.body, false));
		}
	}
}
