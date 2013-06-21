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
import org.apache.commons.lang3.StringEscapeUtils;
import org.holoeverywhere.widget.LinearLayout;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.RRTime;
import org.quantumbadger.redreader.reddit.things.RedditPost;

public final class PostPropertiesDialog extends PropertiesDialog {

	public static PostPropertiesDialog newInstance(final RedditPost post) {

		final PostPropertiesDialog pp = new PostPropertiesDialog();

		final Bundle args = new Bundle();
		args.putParcelable("post", post);
		pp.setArguments(args);

		return pp;
	}

	@Override
	protected String getTitle(Context context) {
		return context.getString(R.string.props_post_title);
	}

	@Override
	protected void prepare(Context context, LinearLayout items) {

		final RedditPost post = getArguments().getParcelable("post");

		items.addView(propView(context, R.string.props_title, StringEscapeUtils.unescapeHtml4(post.title.trim()), true));
		items.addView(propView(context, R.string.props_author, post.author, false));
		items.addView(propView(context, R.string.props_url, post.url, false));
		items.addView(propView(context, R.string.props_created, RRTime.formatDateTime(post.created_utc * 1000, context), false));

		if(post.edited instanceof Long) {
			items.addView(propView(context, R.string.props_edited, RRTime.formatDateTime((Long) post.edited * 1000, context), false));
		} else {
			items.addView(propView(context, R.string.props_edited, R.string.props_never, false));
		}

		items.addView(propView(context, R.string.props_subreddit, post.subreddit, false));
		items.addView(propView(context, R.string.props_score,
				String.format("%d (%d %s, %d %s)",
						post.score,
						post.ups, context.getString(R.string.props_up),
						post.downs, context.getString(R.string.props_down)), false));
		items.addView(propView(context, R.string.props_num_comments, String.valueOf(post.num_comments), false));

		if(post.selftext != null && post.selftext.length() > 0) {
			items.addView(propView(context, R.string.props_self_markdown, post.selftext, false));
		}
	}
}
