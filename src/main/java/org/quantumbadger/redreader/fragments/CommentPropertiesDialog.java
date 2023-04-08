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
import android.widget.LinearLayout;
import androidx.annotation.NonNull;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.reddit.kthings.RedditComment;
import org.quantumbadger.redreader.reddit.kthings.RedditFieldEdited;

public final class CommentPropertiesDialog extends PropertiesDialog {

	public static CommentPropertiesDialog newInstance(final RedditComment comment) {

		final CommentPropertiesDialog pp = new CommentPropertiesDialog();

		final Bundle args = new Bundle();
		args.putParcelable("comment", comment);
		pp.setArguments(args);

		return pp;
	}

	@Override
	protected String getTitle(final Context context) {
		return context.getString(R.string.props_comment_title);
	}

	@Override
	protected void prepare(
			@NonNull final BaseActivity context,
			@NonNull final LinearLayout items) {

		final RedditComment comment = getArguments().getParcelable("comment");

		items.addView(propView(context, "ID", comment.getName().getValue(), true));

		// TODO nullability

		items.addView(propView(
				context,
				R.string.props_author,
				comment.getAuthor().getDecoded(),
				false));

		if(comment.getAuthor_flair_text() != null
				&& !comment.getAuthor_flair_text().getDecoded().isEmpty()) {
			items.addView(propView(
					context,
					R.string.props_author_flair,
					comment.getAuthor_flair_text().getDecoded(),
					false));
		}

		items.addView(propView(
				context,
				R.string.props_created,
				comment.getCreated_utc().getValue().format(),
				false));

		if(comment.getEdited() instanceof RedditFieldEdited.Timestamp) {
			items.addView(propView(
					context,
					R.string.props_edited,
					((RedditFieldEdited.Timestamp)comment.getEdited())
							.getValue().getValue().format(),
					false));
		} else {
			items.addView(propView(
					context,
					R.string.props_edited,
					R.string.props_never,
					false));
		}

		items.addView(propView(
				context,
				R.string.props_score,
				String.valueOf(comment.getUps() - comment.getDowns()),
				false));

		items.addView(propView(
				context,
				R.string.props_subreddit,
				comment.getSubreddit().getDecoded(),
				false));

		if(comment.getBody() != null && !comment.getBody().getDecoded().isEmpty()) {
			items.addView(propView(
					context,
					R.string.props_body_markdown,
					comment.getBody().getDecoded(),
					false));

			if(comment.getBody_html() != null) {
				items.addView(propView(
						context,
						R.string.props_body_html,
						comment.getBody_html().getDecoded(),
						false));
			}
		}
	}
}
