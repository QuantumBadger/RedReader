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

package org.saiditnet.redreader.reddit.things;

import org.saiditnet.redreader.jsonwrap.JsonBufferedArray;
import org.saiditnet.redreader.jsonwrap.JsonValue;
import org.saiditnet.redreader.reddit.url.PostCommentListingURL;
import org.saiditnet.redreader.reddit.url.RedditURLParser;

import java.util.ArrayList;
import java.util.List;

public class RedditMoreComments {
	public int count;
	public JsonBufferedArray children;
	public String parent_id;

	public List<PostCommentListingURL> getMoreUrls(final RedditURLParser.RedditURL commentListingURL) {

		final ArrayList<PostCommentListingURL> urls = new ArrayList<>(16);

		if(commentListingURL.pathType() == RedditURLParser.POST_COMMENT_LISTING_URL) {

			if(count > 0) {
				for(JsonValue child : children) {
					if(child.getType() == JsonValue.TYPE_STRING) {
						urls.add(commentListingURL.asPostCommentListURL().commentId(child.asString()));
					}
				}

			} else {
				urls.add(commentListingURL.asPostCommentListURL().commentId(parent_id));
			}
		}

		return urls;
	}

	public int getCount() {
		return count;
	}
}
