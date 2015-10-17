package com.konneh.scroll.image;

import org.apache.http.StatusLine;
import com.konneh.scroll.cache.RequestFailureType;

public interface GetAlbumInfoListener {

	void onFailure(
			final RequestFailureType type,
			final Throwable t,
			final StatusLine status,
			final String readableMessage);

	void onSuccess(ImgurAPI.AlbumInfo info);
}
