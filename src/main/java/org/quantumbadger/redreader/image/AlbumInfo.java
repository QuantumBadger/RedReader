package org.quantumbadger.redreader.image;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.apache.commons.text.StringEscapeUtils;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedArray;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

import java.io.IOException;
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
			final JsonBufferedObject object) throws IOException, InterruptedException {

        String title = object.getString("title");
        String description = object.getString("description");

        if(title != null) {
            title = StringEscapeUtils.unescapeHtml4(title);
        }

        if(description != null) {
            description = StringEscapeUtils.unescapeHtml4(description);
        }

        final JsonBufferedArray imagesJson = object.getArray("images");
        final ArrayList<ImageInfo> images = new ArrayList<>();

        for(final JsonValue imageJson : imagesJson) {
            images.add(ImageInfo.parseImgur(imageJson.asObject()));
        }

        return new AlbumInfo(url, title, description, images);
    }

    public static AlbumInfo parseImgurV3(final String url, final JsonBufferedObject object)
            throws IOException, InterruptedException {

        String title = object.getString("title");
        String description = object.getString("description");

        if(title != null) {
            title = StringEscapeUtils.unescapeHtml4(title);
        }

        if(description != null) {
            description = StringEscapeUtils.unescapeHtml4(description);
        }

        final JsonBufferedArray imagesJson = object.getArray("images");
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
	private static String getThumbnail(final JsonBufferedArray images)
			throws IOException, InterruptedException {

    	final int minThumbSize = 200;

    	Integer bestSizeMinAxis = null;
    	String bestUrl = null;

    	for(final JsonValue value : images) {

			final JsonBufferedObject image = value.asObject();

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

	public static AlbumInfo parseRedditGallery(final String url, final JsonBufferedObject object)
			throws IOException, InterruptedException {

		final JsonBufferedObject mediaMetadataList = object.getObject("media_metadata");
		final JsonBufferedArray galleryItems = object.getObject("gallery_data").getArray("items");

		final ArrayList<ImageInfo> images = new ArrayList<>();

		for(final JsonValue itemValue : galleryItems) {

			final JsonBufferedObject item = itemValue.asObject();

			final String mediaId = StringEscapeUtils.unescapeHtml4(item.getString("media_id"));

			@Nullable final String caption
					= StringEscapeUtils.unescapeHtml4(item.getString("caption"));

			// TODO show this in the UI
			@SuppressWarnings("unused")
			@Nullable final String outboundUrl
					= StringEscapeUtils.unescapeHtml4(item.getString("outbound_url"));

			final JsonBufferedObject mediaMetadataEntry = mediaMetadataList.getObject(mediaId);

			@Nullable final String mimetype
					= StringEscapeUtils.unescapeHtml4(mediaMetadataEntry.getString("m"));

			final JsonBufferedObject standardImage = mediaMetadataEntry.getObject("s");

			final ImageInfo.MediaType mediaType = stringToMediaType(mediaMetadataEntry.getString("e"));

			images.add(new ImageInfo(
					StringEscapeUtils.unescapeHtml4(standardImage.getString("u")),
					getThumbnail(mediaMetadataEntry.getArray("p")),
					caption,
					null,
					mimetype,
					mediaType != ImageInfo.MediaType.IMAGE,
					standardImage.getLong("x"),
					standardImage.getLong("y"),
					null,
					mediaType,
					ImageInfo.HasAudio.NO_AUDIO));
		}

		final String title = StringEscapeUtils.unescapeHtml4(object.getString("title"));

		return new AlbumInfo(url, title, null, images);
	}
}
