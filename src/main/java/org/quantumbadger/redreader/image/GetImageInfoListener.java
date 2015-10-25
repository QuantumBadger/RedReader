package org.quantumbadger.redreader.image;

import org.apache.http.StatusLine;
import org.quantumbadger.redreader.cache.RequestFailureType;

public interface GetImageInfoListener {

	void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage);

	void onSuccess(ImageInfo info);

	void onNotAnImage();
}
