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

package org.quantumbadger.redreader.reddit.url;

import android.content.Context;
import android.net.Uri;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.listingcontrollers.CommentListingController;

public class CommentListingURL extends RedditURLParser.RedditURL {

	public final String after;

	public final String postId;
	public final String commentId;

	public final Integer context;
	public final Integer limit;

	public final CommentListingController.Sort order;

	public CommentListingURL(
			final String after,
			final String postId,
			final String commentId,
			final Integer context,
			final Integer limit,
			final CommentListingController.Sort order) {

		this.after = after;
		this.postId = postId;
		this.commentId = commentId;
		this.context = context;
		this.limit = limit;
		this.order = order;
	}


	public CommentListingURL after(String after) {
		return new CommentListingURL(after, postId, commentId, context, limit, order);
	}

	public CommentListingURL limit(Integer limit) {
		return new CommentListingURL(after, postId, commentId, context, limit, order);
	}

	public CommentListingController.Sort getOrder() {
		return order;
	}

	@Override
	public Uri generateJsonUri() {

		Uri.Builder builder = new Uri.Builder();
		builder.scheme(Constants.Reddit.getScheme()).authority(Constants.Reddit.getDomain());

		builder.encodedPath("/comments");
		builder.appendPath(postId);

		if(commentId != null) {

			builder.appendEncodedPath("comment");
			builder.appendPath(commentId);

			if(context != null) {
				builder.appendQueryParameter("context", context.toString());
			}
		}

		if(after != null) {
			builder.appendQueryParameter("after", after);
		}

		if(limit != null) {
			builder.appendQueryParameter("limit", limit.toString());
		}

		if(order != null) {
			builder.appendQueryParameter("sort", order.key);
		}

		builder.appendEncodedPath(".json");

		return builder.build();
	}

	@Override
	public RedditURLParser.PathType pathType() {
		return RedditURLParser.PathType.CommentListingURL;
	}

	@Override
	public String humanReadablePath() {
		return super.humanReadablePath();
	}

	@Override
	public String humanReadableUrl() {
		return super.humanReadableUrl();
	}

	@Override
	public String humanReadableName(final Context context, final boolean shorter) {
		return super.humanReadableName(context, shorter);
	}
}
