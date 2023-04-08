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
import org.quantumbadger.redreader.reddit.kthings.RedditFieldEdited;
import org.quantumbadger.redreader.reddit.kthings.RedditPost;

public final class PostPropertiesDialog extends PropertiesDialog {

	public static PostPropertiesDialog newInstance(final RedditPost post) {

		final PostPropertiesDialog pp = new PostPropertiesDialog();

		final Bundle args = new Bundle();
		args.putParcelable("post", post);
		pp.setArguments(args);

		return pp;
	}

	@Override
	protected String getTitle(final Context context) {
		return context.getString(R.string.props_post_title);
	}

	@Override
	protected void prepare(
			@NonNull final BaseActivity context,
			@NonNull final LinearLayout items) {

		final RedditPost post = getArguments().getParcelable("post");

		// TODO nullability

		items.addView(propView(
				context,
				R.string.props_title,
				post.getTitle().getDecoded().trim(),
				true));
		items.addView(propView(
				context,
				R.string.props_author,
				post.getAuthor().getDecoded(),
				false));
		items.addView(propView(
				context,
				R.string.props_url,
				post.getUrl().getDecoded(),
				false));
		items.addView(propView(
				context,
				R.string.props_created,
				post.getCreated_utc().getValue().format(),
				false));

		if(post.getEdited() instanceof RedditFieldEdited.Timestamp) {
			items.addView(propView(
					context,
					R.string.props_edited,
					((RedditFieldEdited.Timestamp)post.getEdited())
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
				R.string.props_subreddit,
				post.getSubreddit().getDecoded(),
				false));
		items.addView(propView(
				context,
				R.string.props_score,
				String.valueOf(post.getScore()),
				false));
		items.addView(propView(
				context,
				R.string.props_num_comments,
				String.valueOf(post.getNum_comments()),
				false));

		if(post.getSelftext() != null && !post.getSelftext().getDecoded().isEmpty()) {
			items.addView(propView(
					context,
					R.string.props_self_markdown,
					post.getSelftext().getDecoded(),
					false));

			if(post.getSelftext_html() != null) {
				items.addView(propView(
						context,
						R.string.props_self_html,
						post.getSelftext_html().getDecoded(),
						false));
			}
		}
	}
}
