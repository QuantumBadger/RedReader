package org.quantumbadger.redreader.http;

import android.net.Uri;

import java.io.InputStream;

public interface HTTPBackend {

	class RequestDetails {

		private final Uri mUrl;

		public RequestDetails(final Uri url) {
			mUrl = url;
		}

		public Uri getUrl() {
			return mUrl;
		}
	}

	interface Request {

		void executeInThisThread();

		void cancel();
	}

	interface Listener {

		void onError(Throwable exception, Integer httpStatus);

		void onSuccess(
				String mimetype,
				Long bodyBytes,
				InputStream body);
	}

	Request prepareRequest(
			RequestDetails details,
			Listener listener);
}
