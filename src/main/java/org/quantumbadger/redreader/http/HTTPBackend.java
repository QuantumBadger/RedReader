package org.quantumbadger.redreader.http;

import android.content.Context;
import org.quantumbadger.redreader.cache.RequestFailureType;

import java.io.InputStream;
import java.net.URI;
import java.util.List;

public interface HTTPBackend {

	class RequestDetails {

		private final URI mUrl;
		private final List<PostField> mPostFields;

		public RequestDetails(final URI url, final List<PostField> postFields) {
			mUrl = url;
			mPostFields = postFields;
		}

		public URI getUrl() {
			return mUrl;
		}

		public List<PostField> getPostFields() {
			return mPostFields;
		}
	}

	class PostField {

		public final String name;
		public final String value;

		public PostField(final String name, final String value) {
			this.name = name;
			this.value = value;
		}
	}

	interface Request {

		void executeInThisThread(final Listener listener);

		void cancel();

		void addHeader(String name, String value);
	}

	interface Listener {

		void onError(RequestFailureType failureType, Throwable exception, Integer httpStatus);

		void onSuccess(
				String mimetype,
				Long bodyBytes,
				InputStream body);
	}

	Request prepareRequest(
			Context context,
			RequestDetails details);
}
