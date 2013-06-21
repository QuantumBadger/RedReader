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

package org.quantumbadger.redreader.listingcontrollers;

import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;

import java.net.URI;

// TODO add notification/header for abnormal sort order
public class PostListingControllerSubreddit extends PostListingController {

	private final RedditSubreddit subreddit;

	public PostListingControllerSubreddit(final RedditSubreddit subreddit) {
		this.subreddit = subreddit;
	}

	@Override
	public RedditSubreddit getSubreddit() {
		return subreddit;
	}

	@Override
	public boolean isSortable() {
		return subreddit.isSortable();
	}

	// TODO customise limit
	@Override
	public URI getUri() {

		if(!subreddit.isSortable()) {
			if(subreddit.url.contains("?"))
				return Constants.Reddit.getUri(subreddit.url.replaceFirst("\\?", ".json?").replace(".json.json", ".json"));
			else
				return Constants.Reddit.getUri(subreddit.url + ".json");
		}

		switch(getSort()) {
			case HOT:
			case NEW:
			case CONTROVERSIAL:
			case RISING:
				return Constants.Reddit.getUri(subreddit.url + "/" + getSort().name().toLowerCase() + ".json");

			case TOP_HOUR:
			case TOP_DAY:
			case TOP_WEEK:
			case TOP_MONTH:
			case TOP_YEAR:
			case TOP_ALL:
				return Constants.Reddit.getUri(subreddit.url + "/top.json?t=" + getSort().name().split("_")[1].toLowerCase());
		}

		throw new RuntimeException("Unknown sort type " + getSort().name());
	}
}
