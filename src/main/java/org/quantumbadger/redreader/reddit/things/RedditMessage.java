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

import androidx.annotation.Nullable;
import org.apache.commons.text.StringEscapeUtils;
import org.quantumbadger.redreader.jsonwrap.JsonObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

public class RedditMessage implements JsonObject.JsonDeserializable {

	@Nullable public String author;
	public String dest;
	public String body;
	public String body_html;
	public String context;
	public String name;
	public String parent_id;
	public String subject;
	public String subreddit;
	public String subreddit_name_prefixed;
	public boolean _json_new;
	public boolean was_comment;
	public JsonValue first_message;
	public JsonValue replies;
	public long created;
	public long created_utc;

	public String getUnescapedBodyMarkdown() {
		return StringEscapeUtils.unescapeHtml4(body);
	}

	public String getUnescapedBodyHtml() {
		return StringEscapeUtils.unescapeHtml4(body_html);
	}
}
