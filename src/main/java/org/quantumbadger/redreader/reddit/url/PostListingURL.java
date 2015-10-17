/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package com.konneh.scroll.reddit.url;

import com.konneh.scroll.listingcontrollers.PostListingController;

public abstract class PostListingURL extends RedditURLParser.RedditURL {

	public abstract PostListingURL after(String after);
	public abstract PostListingURL limit(Integer limit);

	public PostListingController.Sort getOrder() {
		return null;
	}
}
