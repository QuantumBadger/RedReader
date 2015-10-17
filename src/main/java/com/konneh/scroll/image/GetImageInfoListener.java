package com.konneh.scroll.image;

import org.apache.http.StatusLine;
import com.konneh.scroll.cache.RequestFailureType;

public interface GetImageInfoListener {

	void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage);

	void onSuccess(ImgurAPI.ImageInfo info);

	void onNotAnImage();
}
