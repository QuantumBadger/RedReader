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
import androidx.annotation.NonNull;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.StringUtils;
import org.quantumbadger.redreader.reddit.PostCommentSort;

import java.util.ArrayList;
import java.util.List;

public class PostCommentListingURL extends CommentListingURL {

	public final String after;

	public final String postId;
	public final String commentId;

	public final Integer context;
	public final Integer limit;

	public final PostCommentSort order;

	public static PostCommentListingURL forPostId(final String postId) {
		return new PostCommentListingURL(null, postId, null, null, null, null);
	}

	public PostCommentListingURL(
			final String after,
			String postId,
			String commentId,
			final Integer context,
			final Integer limit,
			final PostCommentSort order) {

		if(postId != null && postId.startsWith("t3_")) {
			postId = postId.substring(3);
		}

		if(commentId != null && commentId.startsWith("t1_")) {
			commentId = commentId.substring(3);
		}

		this.after = after;
		this.postId = postId;
		this.commentId = commentId;
		this.context = context;
		this.limit = limit;
		this.order = order;
	}

	@Override
	public PostCommentListingURL after(final String after) {
		return new PostCommentListingURL(after, postId, commentId, context, limit, order);
	}

	@Override
	public PostCommentListingURL limit(final Integer limit) {
		return new PostCommentListingURL(after, postId, commentId, context, limit, order);
	}

	public PostCommentListingURL context(final Integer context) {
		return new PostCommentListingURL(after, postId, commentId, context, limit, order);
	}

	public PostCommentListingURL order(final PostCommentSort order) {
		return new PostCommentListingURL(after, postId, commentId, context, limit, order);
	}

	public PostCommentListingURL commentId(String commentId) {

		if(commentId != null && commentId.startsWith("t1_")) {
			commentId = commentId.substring(3);
		}

		return new PostCommentListingURL(after, postId, commentId, context, limit, order);
	}

	@Override
	public Uri generateJsonUri() {

		final Uri.Builder builder = new Uri.Builder();
		builder.scheme(Constants.Reddit.getScheme())
				.authority(Constants.Reddit.getDomain());

		internalGenerateCommon(builder);

		builder.appendEncodedPath(".json");

		return builder.build();
	}

	public Uri generateNonJsonUri() {

		final Uri.Builder builder = new Uri.Builder();
		builder.scheme(Constants.Reddit.getScheme())
				.authority(Constants.Reddit.getHumanReadableDomain());
		internalGenerateCommon(builder);
		return builder.build();
	}

	private void internalGenerateCommon(@NonNull final Uri.Builder builder) {

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
	}

	public static PostCommentListingURL parse(final Uri uri) {

		final String[] pathSegments;
		{
			final List<String> pathSegmentsList = uri.getPathSegments();

			final ArrayList<String> pathSegmentsFiltered = new ArrayList<>(
					pathSegmentsList.size());
			for(String segment : pathSegmentsList) {

				while(StringUtils.asciiLowercase(segment).endsWith(".json")
						|| StringUtils.asciiLowercase(segment).endsWith(".xml")) {
					segment = segment.substring(0, segment.lastIndexOf('.'));
				}

				pathSegmentsFiltered.add(segment);
			}

			pathSegments
					= pathSegmentsFiltered.toArray(new String[0]);
		}

		if(pathSegments.length == 1 && uri.getHost().equals("redd.it")) {
			return new PostCommentListingURL(
					null,
					pathSegments[0],
					null,
					null,
					null,
					null);
		}

		if(pathSegments.length < 2) {
			return null;
		}

		int offset = 0;

		if(pathSegments[0].equalsIgnoreCase("r")) {
			offset = 2;

			if(pathSegments.length - offset < 2) {
				return null;
			}
		}

		if(!pathSegments[offset].equalsIgnoreCase("comments")) {
			return null;
		}

		final String postId;
		String commentId = null;

		postId = pathSegments[offset + 1];
		offset += 2;

		if(pathSegments.length - offset >= 2) {
			commentId = pathSegments[offset + 1];
		}

		String after = null;
		Integer limit = null;
		Integer context = null;
		PostCommentSort order = null;

		for(final String parameterKey : General.getUriQueryParameterNames(uri)) {

			if(parameterKey.equalsIgnoreCase("after")) {
				after = uri.getQueryParameter(parameterKey);

			} else if(parameterKey.equalsIgnoreCase("limit")) {
				try {
					limit = Integer.parseInt(uri.getQueryParameter(parameterKey));
				} catch(final Throwable ignored) {
				}

			} else if(parameterKey.equalsIgnoreCase("context")) {
				try {
					context = Integer.parseInt(uri.getQueryParameter(parameterKey));
				} catch(final Throwable ignored) {
				}

			} else if(parameterKey.equalsIgnoreCase("sort")) {
				order = PostCommentSort.lookup(uri.getQueryParameter(parameterKey));
			}
		}

		return new PostCommentListingURL(after, postId, commentId, context, limit, order);
	}

	@Override
	public @RedditURLParser.PathType
	int pathType() {
		return RedditURLParser.POST_COMMENT_LISTING_URL;
	}

	@Override
	public String humanReadableName(final Context context, final boolean shorter) {
		return super.humanReadableName(context, shorter);
	}

}
