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

import org.apache.commons.text.StringEscapeUtils;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

public class RedditMessage {

	public String author, body, body_html, context, name, parent_id, subject, subreddit;
	public boolean _json_new, was_comment;
	public JsonValue first_message, replies;
	public long created, created_utc;

	public String getUnescapedBodyMarkdown() {
		return StringEscapeUtils.unescapeHtml4(body);
	}

	public String getUnescapedBodyHtml() {
		return StringEscapeUtils.unescapeHtml4(body_html);
	}
}
