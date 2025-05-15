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

import androidx.annotation.NonNull;
import org.quantumbadger.redreader.jsonwrap.JsonObject;

import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;

public final class RedditThing implements JsonObject.JsonDeserializable {

	public static final String KIND_USER = "t2";

	public enum Kind {
		POST, USER, COMMENT, MESSAGE, SUBREDDIT, MORE_COMMENTS, LISTING
	}

	private static final Map<String, Kind> kinds;

	static {
		kinds = new HashMap<>();
		kinds.put("t1", Kind.COMMENT);
		kinds.put(KIND_USER, Kind.USER);
		kinds.put("t3", Kind.POST);
		kinds.put("t4", Kind.MESSAGE);
		kinds.put("t5", Kind.SUBREDDIT);
		kinds.put("more", Kind.MORE_COMMENTS);
		kinds.put("Listing", Kind.LISTING);
	}

	public String kind;
	public JsonObject data;

	@NonNull
	public Kind getKind() {

		final Kind result = kinds.get(this.kind);

		if(result == null) {
			throw new RuntimeException("Unknown thing type: " + this.kind);
		}

		return result;
	}

	public RedditSubreddit asSubreddit() throws
			InstantiationException,
			IllegalAccessException,
			NoSuchMethodException,
			InvocationTargetException {
		return data.asObject(RedditSubreddit.class);
	}

	public RedditUser asUser() throws
			InstantiationException,
			IllegalAccessException,
			NoSuchMethodException,
			InvocationTargetException {
		return data.asObject(RedditUser.class);
	}
}
