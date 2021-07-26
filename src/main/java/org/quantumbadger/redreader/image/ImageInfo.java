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
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.apache.commons.text.StringEscapeUtils;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.ParcelHelper;
import org.quantumbadger.redreader.jsonwrap.JsonObject;

import java.io.IOException;

public class ImageInfo implements Parcelable {

	public final String urlOriginal;
	public final String urlBigSquare;

	@Nullable public final String urlAudioStream;

	public final String title;
	public final String caption;

	public final String type;
	public final Boolean isAnimated;

	public final Long width;
	public final Long height;
	public final Long size;

	public final MediaType mediaType;
	@NonNull public final HasAudio hasAudio;

	@Nullable public final String urlPreview;
	@Nullable public final Long widthPreview;
	@Nullable public final Long heightPreview;

	public enum MediaType {
		IMAGE, VIDEO, GIF
	}

	public enum HasAudio {
		HAS_AUDIO, MAYBE_AUDIO, NO_AUDIO;

		public static @NonNull HasAudio fromBoolean(@Nullable final Boolean value) {

			if(value == null) {
				return MAYBE_AUDIO;
			}

			if(value) {
				return HAS_AUDIO;
			} else {
				return NO_AUDIO;
			}
		}
	}

	public ImageInfo(
			final String urlOriginal,
			final MediaType mediaType,
			@NonNull final HasAudio hasAudio) {
		this(urlOriginal, null, mediaType, hasAudio);
	}

	public ImageInfo(
			final String urlOriginal,
			@Nullable final String urlAudioStream,
			final MediaType mediaType,
			@NonNull final HasAudio hasAudio) {

		this.urlOriginal = urlOriginal;
		this.urlAudioStream = urlAudioStream;

		urlBigSquare = null;
		title = null;
		caption = null;
		type = null;
		isAnimated = null;
		width = null;
		height = null;
		size = null;
		this.mediaType = mediaType;
		this.hasAudio = hasAudio;
		urlPreview = null;
		widthPreview = null;
		heightPreview = null;
	}

	private ImageInfo(final Parcel in) {
		urlOriginal = ParcelHelper.readNullableString(in);
		urlBigSquare = ParcelHelper.readNullableString(in);
		urlAudioStream = ParcelHelper.readNullableString(in);
		title = ParcelHelper.readNullableString(in);
		caption = ParcelHelper.readNullableString(in);
		type = ParcelHelper.readNullableString(in);
		isAnimated = ParcelHelper.readNullableBoolean(in);
		width = ParcelHelper.readNullableLong(in);
		height = ParcelHelper.readNullableLong(in);
		size = ParcelHelper.readNullableLong(in);
		mediaType = ParcelHelper.readNullableImageInfoMediaType(in);
		hasAudio = ParcelHelper.readImageInfoHasAudio(in);
		urlPreview = ParcelHelper.readNullableString(in);
		widthPreview = ParcelHelper.readNullableLong(in);
		heightPreview = ParcelHelper.readNullableLong(in);
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
			final Long size,
			final MediaType mediaType,
			@NonNull final HasAudio hasAudio,
			@Nullable final String urlPreview,
			@Nullable final Long widthPreview,
			@Nullable final Long heightPreview) {

		this.urlOriginal = urlOriginal;
		this.urlBigSquare = urlBigSquare;
		this.urlAudioStream = null;
		this.title = title;
		this.caption = caption;
		this.type = type;
		this.isAnimated = isAnimated;
		this.width = width;
		this.height = height;
		this.size = size;
		this.mediaType = mediaType;
		this.hasAudio = hasAudio;
		this.urlPreview = urlPreview;
		this.widthPreview = widthPreview;
		this.heightPreview = heightPreview;
	}

	public static ImageInfo parseGfycat(final JsonObject object) {

		final Long width = object.getLong("width");
		final Long height = object.getLong("height");

		final String urlOriginal = object.getString("mp4Url");
		final Long size = object.getLong("mp4Size");

		final String title = object.getString("title");

		@Nullable final Boolean hasAudio = object.getBoolean("hasAudio");

		@Nullable final String urlPreview;
		@Nullable final Long widthPreview;
		@Nullable final Long heightPreview;

		final Optional<JsonObject> mobilePoster
				= object.getObjectAtPath("content_urls", "mobilePoster");

		if(mobilePoster.isPresent()) {
			urlPreview = mobilePoster.get().getString("url");
			widthPreview = mobilePoster.get().getLong("width");
			heightPreview = mobilePoster.get().getLong("height");

		} else {
			urlPreview = null;
			widthPreview = null;
			heightPreview = null;
		}

		return new ImageInfo(
				urlOriginal,
				null,
				title,
				null,
				"video/mp4",
				true,
				width,
				height,
				size,
				MediaType.VIDEO,
				HasAudio.fromBoolean(hasAudio),
				urlPreview,
				widthPreview,
				heightPreview);
	}

	public static ImageInfo parseStreamable(final JsonObject object)
			throws IOException {

		JsonObject fileObj = null;
		final JsonObject files = object.getObject("files");

		final String[] preferredTypes = {
				"mp4",
				"webm",
				"mp4-high",
				"webm-high",
				"mp4-mobile",
				"webm-mobile"};
		String selectedType = null;

		for(final String type : preferredTypes) {
			fileObj = files.getObject(type);
			selectedType = type;
			if(fileObj != null) {
				break;
			}
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
				null,
				MediaType.VIDEO,
				HasAudio.MAYBE_AUDIO,
				null,
				null,
				null);
	}

	public static ImageInfo parseImgur(final JsonObject object) {

		final JsonObject image = object.getObject("image");
		final JsonObject links = object.getObject("links");

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
			if(urlOriginal != null && isAnimated) {
				urlOriginal = urlOriginal.replace(".gif", ".mp4");
			}

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
				size,
				isAnimated ? MediaType.VIDEO : MediaType.IMAGE,
				isAnimated ? HasAudio.MAYBE_AUDIO : HasAudio.NO_AUDIO,
				null,
				null,
				null);
	}

	public static ImageInfo parseImgurV3(final JsonObject object) {

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
		boolean mp4 = false;
		@Nullable Boolean hasSound = null;

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
				mp4 = true;
				size = object.getLong("mp4_size");
				hasSound = object.getBoolean("has_sound");
			} else {
				urlOriginal = object.getString("link");
				hasSound = false;
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
				size,
				mp4 ? MediaType.VIDEO : MediaType.IMAGE,
				HasAudio.fromBoolean(hasSound),
				null,
				null,
				null);
	}

	public static ImageInfo parseDeviantArt(final JsonObject object) {

		String urlOriginal = null;
		String thumbnailUrl = null;
		String title = null;
		String tags = null;
		String type = null;
		Long width = null;
		Long height = null;
		final Long size = (long)0;

		if(object != null) {
			urlOriginal = object.getString("url");
			thumbnailUrl = object.getString("thumbnail_url");
			title = object.getString("title");
			tags = object.getString("tags");
			type = object.getString("imagetype");
			width = object.getLong("width");
			height = object.getLong("height");

		}

		if(title != null) {
			title = StringEscapeUtils.unescapeHtml4(title);
		}

		if(tags != null) {
			tags = StringEscapeUtils.unescapeHtml4(tags);
		}

		return new ImageInfo(
				urlOriginal,
				thumbnailUrl,
				title,
				tags,
				type,
				false,
				width,
				height,
				size,
				MediaType.IMAGE,
				HasAudio.NO_AUDIO,
				null,
				null,
				null);
	}

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(final Parcel parcel, final int flags) {

		ParcelHelper.writeNullableString(parcel, urlOriginal);
		ParcelHelper.writeNullableString(parcel, urlBigSquare);
		ParcelHelper.writeNullableString(parcel, urlAudioStream);
		ParcelHelper.writeNullableString(parcel, title);
		ParcelHelper.writeNullableString(parcel, caption);
		ParcelHelper.writeNullableString(parcel, type);
		ParcelHelper.writeNullableBoolean(parcel, isAnimated);
		ParcelHelper.writeNullableLong(parcel, width);
		ParcelHelper.writeNullableLong(parcel, height);
		ParcelHelper.writeNullableLong(parcel, size);
		ParcelHelper.writeNullableEnum(parcel, mediaType);
		ParcelHelper.writeNonNullEnum(parcel, hasAudio);
		ParcelHelper.writeNullableString(parcel, urlPreview);
		ParcelHelper.writeNullableLong(parcel, widthPreview);
		ParcelHelper.writeNullableLong(parcel, heightPreview);
	}

	public static final Parcelable.Creator<ImageInfo> CREATOR
			= new Parcelable.Creator<ImageInfo>() {
		@Override
		public ImageInfo createFromParcel(final Parcel in) {
			return new ImageInfo(in);
		}

		@Override
		public ImageInfo[] newArray(final int size) {
			return new ImageInfo[size];
		}
	};
}
