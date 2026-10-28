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

package org.quantumbadger.redreader.reddit.prepared.html;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;

import org.quantumbadger.redreader.common.UriString;
import org.quantumbadger.redreader.reddit.prepared.InlineMedia;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementInlineMedia;

import java.util.ArrayList;

/**
 * A link to media attached to the comment, which is displayed as the media itself. The text
 * of the link is normally just its URL (or, for a GIF from Giphy, a small image), so it isn't
 * displayed, and no link button is added for it.
 */
public class HtmlRawElementInlineMedia extends HtmlRawElement {

	@NonNull private final UriString mUrl;
	@NonNull private final InlineMedia mMedia;
	@Nullable private final String mCaption;
	@NonNull private final InlineMediaCollector mCollector;

	public HtmlRawElementInlineMedia(
			@NonNull final ArrayList<HtmlRawElement> children,
			@NonNull final UriString url,
			@NonNull final InlineMedia media,
			@NonNull final InlineMediaCollector collector) {

		mUrl = url;
		mMedia = media;
		mCollector = collector;

		final StringBuilder text = new StringBuilder();

		for(final HtmlRawElement child : children) {
			child.getPlainText(text);
		}

		final String caption = text.toString().trim();

		mCaption = caption.isEmpty() || caption.equals(url.value) ? null : caption;
	}

	@Override
	public void getPlainText(@NonNull final StringBuilder stringBuilder) {
		if(mCaption != null) {
			stringBuilder.append(mCaption);
		}
	}

	@Override
	public void reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<HtmlRawElement> destination,
			@NonNull final ArrayList<LinkButtonDetails> linkButtons) {

		destination.add(this);
	}

	@Override
	public void generate(
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<BodyElement> destination) {

		final BodyElementInlineMedia element
				= new BodyElementInlineMedia(mUrl, mMedia, mCaption);

		mCollector.onGenerated(element);
		destination.add(element);
	}
}
