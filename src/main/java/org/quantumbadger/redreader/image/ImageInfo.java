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

package org.quantumbadger.redreader.image;

import org.apache.commons.lang3.StringEscapeUtils;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;

import java.io.IOException;

public class ImageInfo {

	public final String urlOriginal;
	public final String urlBigSquare;

	public final String title;
	public final String caption;

	public final String type;
	public final Boolean isAnimated;

	public final Long width;
	public final Long height;
	public final Long size;

	public ImageInfo(final String urlOriginal) {

		this.urlOriginal = urlOriginal;

		urlBigSquare = null;
		title = null;
		caption = null;
		type = null;
		isAnimated = null;
		width = null;
		height = null;
		size = null;
	}

	public ImageInfo(
			final String urlOriginal,
			final String urlBigSquare,
			final String title,
			final String caption,
			final String type,
			final Boolean isAnimated,
			final Long width,
			final Long height,
			final Long size) {

		this.urlOriginal = urlOriginal;
		this.urlBigSquare = urlBigSquare;
		this.title = title;
		this.caption = caption;
		this.type = type;
		this.isAnimated = isAnimated;
		this.width = width;
		this.height = height;
		this.size = size;
	}

	public static ImageInfo parseGfycat(final JsonBufferedObject object)
			throws IOException, InterruptedException {

		final Long width = object.getLong("width");
		final Long height = object.getLong("height");

		final String urlOriginal = object.getString("mp4Url");
		final Long size = object.getLong("mp4Size");

		final String title = object.getString("title");

		return new ImageInfo(
				urlOriginal,
				null,
				title,
				null,
				"video/mp4",
				true,
				width,
				height,
				size);
	}

	public static ImageInfo parseImgur(final JsonBufferedObject object)
			throws IOException, InterruptedException {

		final JsonBufferedObject image = object.getObject("image");
		final JsonBufferedObject links = object.getObject("links");

		String urlOriginal = null;
		String urlBigSquare = null;
		String title = null;
		String caption = null;
		String type = null;
		boolean isAnimated = false;
		Long width = null;
		Long height = null;
		Long size = null;

		if(image != null) {
			title = image.getString("title");
			caption = image.getString("caption");
			type = image.getString("type");
			isAnimated = "true".equals(image.getString("animated"));
			width = image.getLong("width");
			height = image.getLong("height");
			size = image.getLong("size");
		}

		if(links != null) {
			urlOriginal = links.getString("original");
			if(urlOriginal != null && isAnimated) urlOriginal = urlOriginal.replace(".gif", ".webm");

			urlBigSquare = links.getString("big_square");
		}

		if(title != null) {
			title = StringEscapeUtils.unescapeHtml4(title);
		}

		if(caption != null) {
			caption = StringEscapeUtils.unescapeHtml4(caption);
		}

		return new ImageInfo(
				urlOriginal,
				urlBigSquare,
				title,
				caption,
				type,
				isAnimated,
				width,
				height,
				size);
	}
}
