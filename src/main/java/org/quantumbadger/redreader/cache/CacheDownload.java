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

package org.quantumbadger.redreader.cache;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.protocol.ClientContext;
import org.apache.http.protocol.BasicHttpContext;
import org.apache.http.protocol.HTTP;
import org.apache.http.protocol.HttpContext;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.common.RRTime;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.LinkedList;
import java.util.UUID;

// TODO notify late joiners of latest progress
public final class CacheDownload {

	public final CacheRequest initiator;
	private final CacheManager manager;
	private final LinkedList<CacheRequest> lateJoiners = new LinkedList<CacheRequest>();
	private final UUID session;

	private boolean cancelled = false;
	private HttpGet httpGet = null;

	private volatile CacheRequest highestPriorityReq;

	private boolean success = false;
	private CacheManager.WritableCacheFile cacheFile = null;

	private JsonValue value = null;
	private String mimetype;

	private final PrioritisedDownloadQueue queue;

	public CacheDownload(final CacheRequest initiator, final CacheManager manager, final PrioritisedDownloadQueue queue) {

		this.initiator = initiator;

		this.manager = manager;
		this.queue = queue;
		highestPriorityReq = initiator;

		if(!initiator.setDownload(this)) {
			cancel();
		}

		if(initiator.requestSession != null) {
			session = initiator.requestSession;
		} else {
			session = UUID.randomUUID();
		}
	}

	public synchronized void cancel() {
		cancelled = true;
		if(httpGet != null) httpGet.abort();
		queue.exterminateDownload(this);
		notifyAllOnFailure(RequestFailureType.CANCELLED, null, null, "Cancelled");
	}

	// TODO potential concurrency problem -- late joiner may be added after failure
	public synchronized void addLateJoiner(final CacheRequest request) {

		if(cancelled) {
			request.notifyFailure(RequestFailureType.CANCELLED, null, null, "Cancelled");
			return;
		}

		if(!request.setDownload(this)) {
			notifyAllOnFailure(RequestFailureType.CANCELLED, null, null, "Cancelled");
			return;
		}

		if(request.isJson != initiator.isJson) {
			BugReportActivity.handleGlobalError(request.context, "Late joiner disagrees with initiator on JSON type");
			return;
		}

		lateJoiners.add(request);

		if(request.isHigherPriorityThan(highestPriorityReq)) {
			highestPriorityReq = request;
		}

		if(request.isJson) {
			if(value != null) request.notifyJsonParseStarted(value, RRTime.utcCurrentTimeMillis(), session, false);
		}
	}

	public void doDownload() {

		if(cancelled) {
			queue.removeDownload(this);
			notifyAllOnFailure(RequestFailureType.CANCELLED, null, null, "Cancelled");
			return;
		}

		notifyAllDownloadStarted();

		if(initiator.postFields != null) {

			try {
				downloadPost(queue.getHttpClient());
			} catch(Throwable t) {
				BugReportActivity.handleGlobalError(initiator.context, t);
			} finally {
				queue.removeDownload(this);
			}

		} else {

			try {
				downloadGet(queue.getHttpClient());
			} catch(Throwable t) {
				BugReportActivity.handleGlobalError(initiator.context, t);
			} finally {
				queue.removeDownload(this);
				finishGet();
			}
		}
	}

	// TODO merge with downloadGet
	private void downloadPost(final HttpClient httpClient) {

		final HttpPost httpPost = new HttpPost(initiator.url);

		try {
			httpPost.setEntity(new UrlEncodedFormEntity(initiator.postFields, HTTP.UTF_8));
		} catch (UnsupportedEncodingException e) {
			BugReportActivity.handleGlobalError(initiator.context, e);
			return;
		}

		final HttpContext localContext = new BasicHttpContext();
		localContext.setAttribute(ClientContext.COOKIE_STORE, initiator.getCookies());

		final HttpResponse response;
		final StatusLine status;

		try {
			response = httpClient.execute(httpPost, localContext);
			status = response.getStatusLine();

		} catch(Throwable t) {
			t.printStackTrace();
			notifyAllOnFailure(RequestFailureType.CONNECTION, t, null, "Unable to open a connection");
			return;
		}

		if(status.getStatusCode() != 200) {
			notifyAllOnFailure(RequestFailureType.REQUEST, null, status, String.format("HTTP error %d (%s)", status.getStatusCode(), status.getReasonPhrase()));
			return;
		}

		final HttpEntity entity = response.getEntity();

		if(entity == null) {
			notifyAllOnFailure(RequestFailureType.CONNECTION, null, status, "Did not receive a valid HTTP response");
			return;
		}

		final InputStream is;

		try {
			is = entity.getContent();
		} catch (Throwable t) {
			t.printStackTrace();
			notifyAllOnFailure(RequestFailureType.CONNECTION, t, status, "Could not open an input stream");
			return;
		}

		if(initiator.isJson) {

			final BufferedInputStream bis = new BufferedInputStream(is, 8 * 1024);

			final JsonValue value;

			try {
				value = new JsonValue(bis);
				value.buildInNewThread();

			} catch (Throwable t) {
				t.printStackTrace();
				notifyAllOnFailure(RequestFailureType.PARSE, t, null, "Error parsing the JSON stream");
				return;
			}

			synchronized(this) {
				this.value = value;
				notifyAllOnJsonParseStarted(value, RRTime.utcCurrentTimeMillis(), session);
			}

			try {
				value.join();

			} catch (Throwable t) {
				t.printStackTrace();
				notifyAllOnFailure(RequestFailureType.PARSE, t, null, "Error parsing the JSON stream");
				return;
			}

			success = true;

		} else {
			throw new RuntimeException("POST requests must be for JSON values");
		}

	}

	private void downloadGet(final HttpClient httpClient) {

		httpGet = new HttpGet(initiator.url);
		if(initiator.isJson) httpGet.setHeader("Accept-Encoding", "gzip");

		final HttpContext localContext = new BasicHttpContext();
		localContext.setAttribute(ClientContext.COOKIE_STORE, initiator.getCookies());

		final HttpResponse response;
		final StatusLine status;

		try {
			if(cancelled) {
				notifyAllOnFailure(RequestFailureType.CANCELLED, null, null, "Cancelled");
				return;
			}
			response = httpClient.execute(httpGet, localContext);
			status = response.getStatusLine();

		} catch(Throwable t) {
			t.printStackTrace();
			notifyAllOnFailure(RequestFailureType.CONNECTION, t, null, "Unable to open a connection");
			return;
		}

		if(status.getStatusCode() != 200) {
			notifyAllOnFailure(RequestFailureType.REQUEST, null, status, String.format("HTTP error %d (%s)", status.getStatusCode(), status.getReasonPhrase()));
			return;
		}

		if(cancelled) {
			notifyAllOnFailure(RequestFailureType.CANCELLED, null, null, "Cancelled");
			return;
		}

		final HttpEntity entity = response.getEntity();

		if(entity == null) {
			notifyAllOnFailure(RequestFailureType.CONNECTION, null, status, "Did not receive a valid HTTP response");
			return;
		}

		final InputStream is;

		try {
			is = entity.getContent();
			mimetype = entity.getContentType() == null ? null : entity.getContentType().getValue();
		} catch (Throwable t) {
			t.printStackTrace();
			notifyAllOnFailure(RequestFailureType.CONNECTION, t, status, "Could not open an input stream");
			return;
		}

		final NotifyOutputStream cacheOs;
		if(initiator.cache) {
			try {
				cacheFile = manager.openNewCacheFile(initiator, session, mimetype);
				cacheOs = cacheFile.getOutputStream();
			} catch (IOException e) {
				e.printStackTrace();
				notifyAllOnFailure(RequestFailureType.STORAGE, e, null, "Could not access the local cache");
				return;
			}
		} else {
			cacheOs = null;
		}

		final long contentLength = entity.getContentLength();

		if(initiator.isJson) {

			final InputStream bis;

			if(initiator.cache) {
				final CachingInputStream cis = new CachingInputStream(is, cacheOs, new CachingInputStream.BytesReadListener() {
					public void onBytesRead(final long total) {
						notifyAllOnProgress(total, contentLength);
					}
				});

				bis = new BufferedInputStream(cis, 8 * 1024);
			} else {
				bis = new BufferedInputStream(is, 8 * 1024);
			}

			final JsonValue value;

			try {
				value = new JsonValue(bis);
				value.buildInNewThread();

			} catch (Throwable t) {
				t.printStackTrace();
				notifyAllOnFailure(RequestFailureType.PARSE, t, null, "Error parsing the JSON stream");
				return;
			}

			synchronized(this) {
				this.value = value;
				notifyAllOnJsonParseStarted(value, RRTime.utcCurrentTimeMillis(), session);
			}

			try {
				value.join();

			} catch (Throwable t) {
				t.printStackTrace();
				notifyAllOnFailure(RequestFailureType.PARSE, t, null, "Error parsing the JSON stream");
				return;
			}

			success = true;

		} else {

			if(!initiator.cache) {
				BugReportActivity.handleGlobalError(initiator.context, "Cache disabled for non-JSON request");
				return;
			}

			try {
				final byte[] buf = new byte[8 * 1024];

				int bytesRead;
				long totalBytesRead = 0;
				while((bytesRead = is.read(buf)) > 0) {
					totalBytesRead += bytesRead;
					cacheOs.write(buf, 0, bytesRead);
					notifyAllOnProgress(totalBytesRead, contentLength);
				}

				cacheOs.flush();
				cacheOs.close();
				success = true;

			} catch(Throwable t) {
				t.printStackTrace();
				notifyAllOnFailure(RequestFailureType.CONNECTION, t, null, "The connection was interrupted");
			}
		}
	}

	private synchronized void notifyAllOnFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {

		initiator.notifyFailure(type, t, status, readableMessage);

		for(final CacheRequest req : lateJoiners) {
			req.notifyFailure(type, t, status, readableMessage);
		}
	}

	private synchronized void notifyAllOnProgress(final long bytesRead, final long bytesTotal) {

		initiator.notifyProgress(bytesRead, bytesTotal);

		for(final CacheRequest req : lateJoiners) {
			req.notifyProgress(bytesRead, bytesTotal);
		}
	}

	private synchronized void notifyAllOnJsonParseStarted(final JsonValue value, final long timestamp, final UUID session) {

		initiator.notifyJsonParseStarted(value, timestamp, session, false);

		for(final CacheRequest req : lateJoiners) {
			req.notifyJsonParseStarted(value, timestamp, session, false);
		}
	}

	private synchronized void notifyAllOnSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final String mimetype) {

		initiator.notifySuccess(cacheFile, timestamp, session, false, mimetype);

		for(final CacheRequest req : lateJoiners) {
			req.notifySuccess(cacheFile, timestamp, session, false, mimetype);
		}
	}

	private synchronized void notifyAllDownloadStarted() {

		initiator.notifyDownloadStarted();

		for(final CacheRequest req : lateJoiners) {
			req.notifyDownloadStarted();
		}
	}

	public synchronized boolean isHigherPriorityThan(final CacheDownload another) {
		return highestPriorityReq.isHigherPriorityThan(another.highestPriorityReq);
	}

	private synchronized void finishGet() {

		if(success && initiator.cache) {

			if(cacheFile == null) {
				throw new RuntimeException("Cache file was null, but success was true");
			}

			if(session == null) {
				throw new RuntimeException("Session was null, but success was true");
			}

			notifyAllOnSuccess(cacheFile.getReadableCacheFile(), RRTime.utcCurrentTimeMillis(), session, mimetype);
		}
	}

	public RequestIdentifier createIdentifier() {
		return initiator.createIdentifier();
	}
}