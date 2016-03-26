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

package org.quantumbadger.redreader.reddit.things;

import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.Hashtable;


public final class RedditThing {

	public static enum Kind {
		POST, USER, COMMENT, MESSAGE, SUBREDDIT, MORE_COMMENTS, LISTING
	}
	
	private static final Hashtable<String, Kind> kinds;
	
	static {
		kinds = new Hashtable<>();
		kinds.put("t1", Kind.COMMENT);
		kinds.put("t2", Kind.USER);
		kinds.put("t3", Kind.POST);
		kinds.put("t4", Kind.MESSAGE);
		kinds.put("t5", Kind.SUBREDDIT);
		kinds.put("more", Kind.MORE_COMMENTS);
		kinds.put("Listing", Kind.LISTING);
	}
	
	public String kind;
	public JsonBufferedObject data;
	
	public Kind getKind() {
		return kinds.get(kind);
	}

	public RedditMoreComments asMoreComments() throws InstantiationException, IllegalAccessException, InterruptedException, IOException, NoSuchMethodException, InvocationTargetException {
		return data.asObject(RedditMoreComments.class);
	}

	public RedditComment asComment() throws InstantiationException, IllegalAccessException, InterruptedException, IOException, NoSuchMethodException, InvocationTargetException {
		return data.asObject(RedditComment.class);
	}
	
	public RedditPost asPost() throws InstantiationException, IllegalAccessException, InterruptedException, IOException, NoSuchMethodException, InvocationTargetException {
		return data.asObject(RedditPost.class);
	}

	public RedditSubreddit asSubreddit() throws InstantiationException, IllegalAccessException, InterruptedException, IOException, NoSuchMethodException, InvocationTargetException {
		return data.asObject(RedditSubreddit.class);
	}

	public RedditUser asUser() throws InstantiationException, IllegalAccessException, InterruptedException, IOException, NoSuchMethodException, InvocationTargetException {
		return data.asObject(RedditUser.class);
	}

	public RedditMessage asMessage() throws IllegalAccessException, InterruptedException, InstantiationException, InvocationTargetException, NoSuchMethodException, IOException {
		return data.asObject(RedditMessage.class);
	}
}
