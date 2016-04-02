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

import android.os.Parcel;
import android.os.Parcelable;
import org.apache.commons.lang3.StringEscapeUtils;
import org.quantumbadger.redreader.common.ParcelHelper;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;

import java.io.IOException;

public class ImageInfo implements Parcelable {

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

	private ImageInfo(final Parcel in) {
		urlOriginal = ParcelHelper.readNullableString(in);
		urlBigSquare = ParcelHelper.readNullableString(in);
		title = ParcelHelper.readNullableString(in);
		caption = ParcelHelper.readNullableString(in);
		type = ParcelHelper.readNullableString(in);
		isAnimated = ParcelHelper.readNullableBoolean(in);
		width = ParcelHelper.readNullableLong(in);
		height = ParcelHelper.readNullableLong(in);
		size = ParcelHelper.readNullableLong(in);
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

	public static ImageInfo parseStreamable(final JsonBufferedObject object)
			throws IOException, InterruptedException {

		JsonBufferedObject fileObj = null;
		final JsonBufferedObject files = object.getObject("files");

		final String[] preferredTypes = {"mp4", "webm", "mp4-high", "webm-high", "mp4-mobile", "webm-mobile"};
		String selectedType = null;

		for(final String type : preferredTypes) {
			fileObj = files.getObject(type);
			selectedType = type;
			if(fileObj != null) break;
		}

		if(fileObj == null) {
			throw new IOException("No suitable Streamable files found");
		}

		final String mimeType = "video/" + selectedType.split("\\-")[0];

		final Long width = fileObj.getLong("width");
		final Long height = fileObj.getLong("height");
		String urlOriginal = fileObj.getString("url");

		if(urlOriginal.startsWith("//")) {
			urlOriginal = "https:" + urlOriginal;
		}

		return new ImageInfo(
				urlOriginal,
				null,
				null,
				null,
				mimeType,
				true,
				width,
				height,
				null);
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
			if(urlOriginal != null && isAnimated) urlOriginal = urlOriginal.replace(".gif", ".mp4");

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

	public static ImageInfo parseImgurV3(final JsonBufferedObject object)
			throws IOException, InterruptedException {

		String id = null;
		String urlOriginal = null;
		String thumbnailUrl = null;
		String title = null;
		String caption = null;
		String type = null;
		boolean isAnimated = false;
		Long width = null;
		Long height = null;
		Long size = null;

		if(object != null) {
			id = object.getString("id");
			title = object.getString("title");
			caption = object.getString("description");
			type = object.getString("type");
			isAnimated = object.getBoolean("animated");
			width = object.getLong("width");
			height = object.getLong("height");
			size = object.getLong("size");

			if(object.getString("mp4") != null) {
				urlOriginal = object.getString("mp4");

			} else {
				urlOriginal = object.getString("link");
			}
		}

		if(title != null) {
			title = StringEscapeUtils.unescapeHtml4(title);
		}

		if(caption != null) {
			caption = StringEscapeUtils.unescapeHtml4(caption);
		}

		if(id != null) {
			thumbnailUrl = "https://i.imgur.com/" + id + "b.jpg";
		}

		return new ImageInfo(
				urlOriginal,
				thumbnailUrl,
				title,
				caption,
				type,
				isAnimated,
				width,
				height,
				size);
	}

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(final Parcel parcel, final int flags) {

		ParcelHelper.writeNullableString(parcel, urlOriginal);
		ParcelHelper.writeNullableString(parcel, urlBigSquare);
		ParcelHelper.writeNullableString(parcel, title);
		ParcelHelper.writeNullableString(parcel, caption);
		ParcelHelper.writeNullableString(parcel, type);
		ParcelHelper.writeNullableBoolean(parcel, isAnimated);
		ParcelHelper.writeNullableLong(parcel, width);
		ParcelHelper.writeNullableLong(parcel, height);
		ParcelHelper.writeNullableLong(parcel, size);
	}

	public static final Parcelable.Creator<ImageInfo> CREATOR = new Parcelable.Creator<ImageInfo>() {
		public ImageInfo createFromParcel(final Parcel in) {
			return new ImageInfo(in);
		}

		public ImageInfo[] newArray(final int size) {
			return new ImageInfo[size];
		}
	};
}
