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

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import org.apache.commons.text.StringEscapeUtils;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedArray;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.html.HtmlReader;
import org.quantumbadger.redreader.reddit.things.RedditPost;
import org.quantumbadger.redreader.reddit.things.RedditThingWithIdAndType;

public class RedditParsedPost implements RedditThingWithIdAndType {

	private final RedditPost mSrc;

	private final String mTitle;
	private final String mUrl;
	private final String mPermalink;
	private final BodyElement mSelfText;
	private final String mFlairText;

	public RedditParsedPost(
			@NonNull final AppCompatActivity activity,
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

		if(parseSelfText
				&& src.is_self
				&& src.selftext_html != null
				&& src.selftext.trim().length() > 0) {
			mSelfText = HtmlReader.parse(
					StringEscapeUtils.unescapeHtml4(src.selftext_html),
					activity);
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

	@Nullable
	public String getThumbnailUrl() {
		return StringEscapeUtils.unescapeHtml4(mSrc.thumbnail);
	}

	public boolean isPreviewEnabled() {
		return mSrc.preview != null && Boolean.TRUE.equals(mSrc.preview.getBoolean("enabled"));
	}

	@Nullable
	public String getPreviewUrl(final int minWidth, final int minHeight) {

		if(mSrc.preview == null) {
			return null;
		}

		final JsonBufferedArray images = mSrc.preview.getArray("images");

		if(images == null || images.size() < 1) {
			return null;
		}

		final JsonBufferedObject firstImage = images.getObject(0);

		if(firstImage == null) {
			return null;
		}

		final JsonBufferedArray resolutions = firstImage.getArray("resolutions");

		if(resolutions == null || resolutions.size() < 1) {
			return null;
		}

		int bestWidth = 0;
		int bestHeight = 0;
		String bestUrl = null;

		for(int i = 0; i < resolutions.size(); i++) {

			final JsonBufferedObject resolution = resolutions.getObject(i);

			if(resolution == null) {
				continue;
			}

			final Long width = resolution.getLong("width");
			final Long height = resolution.getLong("height");
			final String url = resolution.getString("url");

			if(width == null || height == null || url == null) {
				continue;
			}

			boolean use = false;

			if(bestUrl == null) {
				use = true;

			} else if((bestWidth < minWidth || bestHeight < minHeight)
					&& (width > bestWidth || height > bestHeight)) {
				use = true;

			} else if(width < bestWidth && height < bestHeight
					&& width >= minWidth && height >= minHeight) {
				use = true;
			}

			if(use) {
				bestWidth = width.intValue();
				bestHeight = height.intValue();
				bestUrl = url;
			}
		}

		return StringEscapeUtils.unescapeHtml4(bestUrl);
	}

	public boolean isArchived() {
		return mSrc.archived;
	}

	public String getAuthor() {
		return mSrc.author;
	}

	public String getRawSelfTextMarkdown() {
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

		if(Boolean.TRUE.equals(mSrc.likes)) {
			score--;
		}
		if(Boolean.FALSE.equals(mSrc.likes)) {
			score++;
		}

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

	public BodyElement getSelfText() {
		return mSelfText;
	}
}
