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

import org.quantumbadger.redreader.reddit.prepared.InlineMedia;
import org.quantumbadger.redreader.reddit.prepared.InlineMediaResolver;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementInlineMedia;

import java.util.ArrayList;
import java.util.List;

/**
 * Passed to {@link HtmlReader#parse(String, androidx.appcompat.app.AppCompatActivity,
 * InlineMediaCollector)} to have links to media attached to a comment displayed as the media
 * itself, rather than as links. Collects the elements displaying that media as they are
 * generated, so that their images can be loaded and released as the comment is scrolled
 * into and out of view.
 */
public final class InlineMediaCollector {

	@NonNull private final InlineMediaResolver mResolver;
	@NonNull private final ArrayList<BodyElementInlineMedia> mGenerated = new ArrayList<>();

	public InlineMediaCollector(@NonNull final InlineMediaResolver resolver) {
		mResolver = resolver;
	}

	@Nullable
	InlineMedia resolve(@NonNull final String href) {
		return mResolver.resolve(href);
	}

	void onGenerated(@NonNull final BodyElementInlineMedia element) {
		mGenerated.add(element);
	}

	@NonNull
	public List<BodyElementInlineMedia> getGenerated() {
		return mGenerated;
	}
}
