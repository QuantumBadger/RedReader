package org.quantumbadger.redreader.image;

import org.quantumbadger.redreader.cache.CacheRequest;

public interface GetAlbumInfoListener {

	void onFailure(
		final @CacheRequest.RequestFailureType int type,
		final Throwable t,
		final Integer status,
		final String readableMessage);

	void onSuccess(ImgurAPI.AlbumInfo info);
}
