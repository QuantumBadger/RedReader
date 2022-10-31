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

package org.saiditnet.redreader.reddit.prepared;

import org.apache.commons.lang3.StringEscapeUtils;
import org.saiditnet.redreader.reddit.prepared.markdown.MarkdownParagraphGroup;
import org.saiditnet.redreader.reddit.prepared.markdown.MarkdownParser;
import org.saiditnet.redreader.reddit.things.RedditPost;
import org.saiditnet.redreader.reddit.things.RedditThingWithIdAndType;

public class RedditParsedPost implements RedditThingWithIdAndType {

	private final RedditPost mSrc;

	private final String mTitle;
	private final String mUrl;
	private final String mPermalink;
	private final MarkdownParagraphGroup mSelfText;
	private final String mFlairText;

	public RedditParsedPost(
			final RedditPost src,
			final boolean parseSelfText) {

		this.mSrc = src;

		if(src.title == null) {
			mTitle = "[null]";
		} else {
			mTitle = StringEscapeUtils.unescapeHtml4(src.title.replace('\n', ' ')).trim();
		}

		mUrl = StringEscapeUtils.unescapeHtml4(src.getUrl());
		mPermalink = StringEscapeUtils.unescapeHtml4(src.permalink);

		if(parseSelfText && src.is_self && src.selftext != null && src.selftext.trim().length() > 0) {
			mSelfText = MarkdownParser.parse(StringEscapeUtils.unescapeHtml4(src.selftext).toCharArray());
		} else {
			mSelfText = null;
		}

		if(src.link_flair_text != null && src.link_flair_text.length() > 0) {
			mFlairText = StringEscapeUtils.unescapeHtml4(src.link_flair_text);
		} else {
			mFlairText = null;
		}
	}

	@Override
	public String getIdAlone() {
		return mSrc.getIdAlone();
	}

	@Override
	public String getIdAndType() {
		return mSrc.getIdAndType();
	}

	public String getTitle() {
		return mTitle;
	}

	public String getUrl() {
		return mUrl;
	}

	public String getPermalink() {
		return mPermalink;
	}

	public boolean isStickied() {
		return mSrc.stickied;
	}

	public RedditPost getSrc() {
		return mSrc;
	}

	public String getThumbnailUrl() {
		return mSrc.thumbnail;
	}

	public boolean isArchived() {
		return mSrc.archived;
	}

	public String getAuthor() {
		return mSrc.author;
	}

	public String getRawSelfText() {
		return mSrc.selftext;
	}

	public boolean isSpoiler() {
		return Boolean.TRUE.equals(mSrc.spoiler);
	}

	public String getUnescapedSelfText() {
		return StringEscapeUtils.unescapeHtml4(mSrc.selftext);
	}

	public String getSubreddit() {
		return mSrc.subreddit;
	}

	public int getScoreExcludingOwnVote() {

		int score = mSrc.score;

		// if(Boolean.TRUE.equals(mSrc.likes)) score--;
		// if(Boolean.FALSE.equals(mSrc.likes)) score++;

		if(Boolean.TRUE.equals(mSrc.likes)) score = score - 2;
		if(Boolean.TRUE.equals(mSrc.dislikes)) score--;

		return score;
	}

	public int getGoldAmount() {
		return mSrc.gilded;
	}

	public boolean isNsfw() {
		return mSrc.over_18;
	}

	public String getFlairText() {
		return mFlairText;
	}

	public long getCreatedTimeSecsUTC() {
		return mSrc.created_utc;
	}

	public String getDomain() {
		return mSrc.domain;
	}

	public boolean isSelfPost() {
		return mSrc.is_self;
	}

	public MarkdownParagraphGroup getSelfText() {
		return mSelfText;
	}
}
