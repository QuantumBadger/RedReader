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

package org.quantumbadger.redreader.reddit.prepared;

import android.content.Context;
import android.support.v7.app.AppCompatActivity;
import org.apache.commons.lang3.StringEscapeUtils;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementRRError;
import org.quantumbadger.redreader.reddit.prepared.html.HtmlReader;
import org.quantumbadger.redreader.reddit.prepared.html.MalformedHtmlException;
import org.quantumbadger.redreader.reddit.things.RedditComment;
import org.quantumbadger.redreader.reddit.things.RedditThingWithIdAndType;

public class RedditParsedComment implements RedditThingWithIdAndType {

	private final RedditComment mSrc;

	private BodyElement mBody;

	private final String mFlair;

	public RedditParsedComment(
			final RedditComment comment,
			final AppCompatActivity activity) {

		mSrc = comment;

		try {
			mBody = HtmlReader.parse(
					StringEscapeUtils.unescapeHtml4(comment.body_html),
					activity);

		} catch(final MalformedHtmlException e) {

			final Context applicationContext = activity.getApplicationContext();

			mBody = new BodyElementRRError(
					new RRError(
							applicationContext.getString(R.string.error_title_malformed_html),
							applicationContext.getString(R.string.error_message_malformed_html),
							e));
		}

		if(comment.author_flair_text != null) {
			mFlair = StringEscapeUtils.unescapeHtml4(comment.author_flair_text);
		} else {
			mFlair = null;
		}
	}

	public BodyElement getBody() {
		return mBody;
	}

	public String getFlair() {
		return mFlair;
	}

	@Override
	public String getIdAlone() {
		return mSrc.getIdAlone();
	}

	@Override
	public String getIdAndType() {
		return mSrc.getIdAndType();
	}

	public RedditComment getRawComment() {
		return mSrc;
	}
}
