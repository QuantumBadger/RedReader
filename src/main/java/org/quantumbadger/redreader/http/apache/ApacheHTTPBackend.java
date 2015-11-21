package org.quantumbadger.redreader.http.apache;

import android.content.Context;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.RedirectException;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.client.protocol.ClientContext;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.protocol.BasicHttpContext;
import org.apache.http.protocol.HTTP;
import org.apache.http.protocol.HttpContext;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.ForgetfulCookieStore;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.http.HTTPBackend;

import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

public class ApacheHTTPBackend implements HTTPBackend {

	class ApacheHTTPRequest implements Request {

		private final HttpClient mClient;
		private final HttpRequestBase mRequest;

		private boolean mCancelled = false;

		ApacheHTTPRequest(final HttpClient client, final HttpRequestBase request) {
			mClient = client;
			mRequest = request;
		}

		@Override
		public void executeInThisThread(final Listener listener) {

			final HttpContext localContext = new BasicHttpContext();
			localContext.setAttribute(ClientContext.COOKIE_STORE, ForgetfulCookieStore.INSTANCE);

			final HttpResponse response;

			try {
				response = mClient.execute(mRequest, localContext);
			} catch(Throwable t) {

				if(t.getCause() != null
						&& t.getCause() instanceof RedirectException
						&& mRequest.getURI().getHost().endsWith("reddit.com")) {

					listener.onError(RequestFailureType.REDDIT_REDIRECT, t, null);

				} else {
					listener.onError(RequestFailureType.CONNECTION, t, null);
				}

				return;
			}

			final StatusLine statusLine = response.getStatusLine();
			final int statusCode = statusLine.getStatusCode();

			if(statusCode != 200 && statusCode != 202) {
				listener.onError(RequestFailureType.REQUEST, null, statusCode);
				return;
			}

			final HttpEntity entity = response.getEntity();

			if(entity == null) {
				listener.onError(RequestFailureType.CONNECTION, null, statusCode);
				return;
			}

			final InputStream is;
			final String mimetype;

			try {
				is = entity.getContent();
				mimetype = entity.getContentType() == null ? null : entity.getContentType().getValue();
			} catch (Throwable t) {
				t.printStackTrace();
				listener.onError(RequestFailureType.CONNECTION, t, statusCode);
				return;
			}

			listener.onSuccess(mimetype, entity.getContentLength(), is);
		}

		@Override
		public synchronized void cancel() {

			if(mCancelled) return;

			new Thread() {
				@Override
				public void run() {
					mRequest.abort();
				}
			}.start();
		}

		@Override
		public void addHeader(final String name, final String value) {
			mRequest.addHeader(name, value);
		}
	}

	@Override
	public Request prepareRequest(
			final Context context,
			final RequestDetails details) {

		final HttpRequestBase httpRequest;

		final List<PostField> postFields = details.getPostFields();

		if(postFields != null) {
			final HttpPost httpPost = new HttpPost(details.getUrl());
			httpRequest = httpPost;

			final ArrayList<NameValuePair> nameValuePairs = new ArrayList<NameValuePair>(postFields.size());

			for(final PostField field : postFields) {
				nameValuePairs.add(new BasicNameValuePair(field.name, field.value));
			}

			try {
				httpPost.setEntity(new UrlEncodedFormEntity(nameValuePairs, HTTP.UTF_8));
			} catch(UnsupportedEncodingException e) {
				throw new RuntimeException(e);
			}

		} else {
			httpRequest = new HttpGet(details.getUrl());
		}

		httpRequest.setHeader("Accept-Encoding", "gzip");

		final HttpClient client = CacheManager.createHttpClient(context);

		return new ApacheHTTPRequest(client, httpRequest);
	}
}
