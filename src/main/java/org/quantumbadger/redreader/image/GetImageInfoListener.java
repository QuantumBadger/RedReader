package org.quantumbadger.redreader.image;

import org.quantumbadger.redreader.cache.RequestFailureType;

public interface GetImageInfoListener {

	void onFailure(final RequestFailureType type, final Throwable t, final Integer status, final String readableMessage);

	void onSuccess(ImageInfo info);

	void onNotAnImage();
}
