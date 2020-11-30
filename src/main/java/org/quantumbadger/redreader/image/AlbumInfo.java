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

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.apache.commons.text.StringEscapeUtils;
import org.quantumbadger.redreader.jsonwrap.JsonArray;
import org.quantumbadger.redreader.jsonwrap.JsonObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

import java.util.ArrayList;

public class AlbumInfo {

	@NonNull public final String url;
	@Nullable public final String title;
	@Nullable public final String description;

	@NonNull public final ArrayList<ImageInfo> images;

	public AlbumInfo(
			@NonNull final String url,
			@Nullable final String title,
			@Nullable final String description,
			@NonNull final ArrayList<ImageInfo> images) {
		this.url = url;
		this.title = title;
		this.description = description;
		this.images = new ArrayList<>(images);
	}

	public static AlbumInfo parseImgur(
			final String url,
			final JsonObject object) {

		String title = object.getString("title");
		String description = object.getString("description");

		if(title != null) {
			title = StringEscapeUtils.unescapeHtml4(title);
		}

		if(description != null) {
			description = StringEscapeUtils.unescapeHtml4(description);
		}

		final JsonArray imagesJson = object.getArray("images");
		final ArrayList<ImageInfo> images = new ArrayList<>();

		for(final JsonValue imageJson : imagesJson) {
			images.add(ImageInfo.parseImgur(imageJson.asObject()));
		}

		return new AlbumInfo(url, title, description, images);
	}

	public static AlbumInfo parseImgurV3(
			final String url,
			final JsonObject object) {

		String title = object.getString("title");
		String description = object.getString("description");

		if(title != null) {
			title = StringEscapeUtils.unescapeHtml4(title);
		}

		if(description != null) {
			description = StringEscapeUtils.unescapeHtml4(description);
		}

		final JsonArray imagesJson = object.getArray("images");
		final ArrayList<ImageInfo> images = new ArrayList<>();

		for(final JsonValue imageJson : imagesJson) {
			images.add(ImageInfo.parseImgurV3(imageJson.asObject()));
		}

		return new AlbumInfo(url, title, description, images);
	}

	@NonNull
	private static ImageInfo.MediaType stringToMediaType(@Nullable final String mediaTypeString) {

		if(mediaTypeString == null) {
			return ImageInfo.MediaType.IMAGE;

		} else {
			switch(mediaTypeString) {

				case "AnimatedImage":
					return ImageInfo.MediaType.GIF;

				case "Video":
					// This string doesn't seem to exist yet, but it might do in future
					return ImageInfo.MediaType.VIDEO;

				case "Image":
				default:
					return ImageInfo.MediaType.IMAGE;
			}
		}
	}

	@Nullable
	private static String getThumbnail(final JsonArray images) {

		final int minThumbSize = 200;

		Integer bestSizeMinAxis = null;
		String bestUrl = null;

		for(final JsonValue value : images) {

			final JsonObject image = value.asObject();

			final int x = (int)(long)image.getLong("x");
			final int y = (int)(long)image.getLong("y");

			final int minAxis = Math.min(x, y);

			if(bestSizeMinAxis == null
					|| (bestSizeMinAxis < minThumbSize && minAxis > bestSizeMinAxis)
					|| (minAxis >= minThumbSize && minAxis < bestSizeMinAxis)) {

				bestSizeMinAxis = minAxis;
				bestUrl = image.getString("u");
			}
		}

		return StringEscapeUtils.unescapeHtml4(bestUrl);
	}

	@Nullable
	public static AlbumInfo parseRedditGallery(
			final String url,
			final JsonObject object) {

		final JsonObject mediaMetadataList = object.getObject("media_metadata");
		final JsonObject galleryData = object.getObject("gallery_data");

		if(mediaMetadataList == null || galleryData == null) {
			return null;
		}

		final JsonArray galleryItems = galleryData.getArray("items");

		final ArrayList<ImageInfo> images = new ArrayList<>();

		for(final JsonValue itemValue : galleryItems) {

			final JsonObject item = itemValue.asObject();

			final String mediaId = StringEscapeUtils.unescapeHtml4(item.getString("media_id"));

			@Nullable final String caption
					= StringEscapeUtils.unescapeHtml4(item.getString("caption"));

			// TODO show this in the UI
			@SuppressWarnings("unused")
			@Nullable final String outboundUrl
					= StringEscapeUtils.unescapeHtml4(item.getString("outbound_url"));

			final JsonObject mediaMetadataEntry = mediaMetadataList.getObject(mediaId);

			@Nullable final String mimetype
					= StringEscapeUtils.unescapeHtml4(mediaMetadataEntry.getString("m"));

			final JsonObject standardImage = mediaMetadataEntry.getObject("s");

			final ImageInfo.MediaType mediaType
					= stringToMediaType(mediaMetadataEntry.getString("e"));

			String urlEscaped = standardImage.getString("u");

			if(urlEscaped == null) {
				urlEscaped = standardImage.getString("mp4");

				if(urlEscaped == null) {
					urlEscaped = standardImage.getString("gif");
				}
			}

			images.add(new ImageInfo(
					StringEscapeUtils.unescapeHtml4(urlEscaped),
					getThumbnail(mediaMetadataEntry.getArray("p")),
					caption,
					null,
					mimetype,
					mediaType != ImageInfo.MediaType.IMAGE,
					standardImage.getLong("x"),
					standardImage.getLong("y"),
					null,
					mediaType,
					ImageInfo.HasAudio.NO_AUDIO,
					null,
					null,
					null));
		}

		final String title = StringEscapeUtils.unescapeHtml4(object.getString("title"));

		return new AlbumInfo(url, title, null, images);
	}
}
