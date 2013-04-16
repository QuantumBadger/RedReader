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

import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.fragments.PostListingFragment;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;

import java.net.URI;
import java.util.UUID;

// TODO add notification/header for abnormal sort order
public abstract class PostListingController {

	private UUID session = null;
	private Sort sort = Sort.HOT; // TODO preference?

	public void setSession(UUID session) {
		this.session = session;
	}

	public UUID getSession() {
		return session;
	}

	public abstract boolean isSortable();

	public static enum Sort {
		HOT, NEW, RISING, TOP_HOUR, TOP_DAY, TOP_WEEK, TOP_MONTH, TOP_YEAR, TOP_ALL, CONTROVERSIAL
	}

	public final void setSort(final Sort s) {
		sort = s;
	}

	public final Sort getSort() {
		return sort;
	}

	public abstract URI getUri();

	public abstract RedditSubreddit getSubreddit();

	public final PostListingFragment get(final boolean force) {
		if(force) session = null;
		return PostListingFragment.newInstance(getSubreddit(), getUri(), session, force ? CacheRequest.DownloadType.FORCE : CacheRequest.DownloadType.IF_NECESSARY);
	}
}
