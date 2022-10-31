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

package org.saiditnet.redreader.http;

import android.content.Context;
import android.util.Log;

import org.saiditnet.redreader.cache.CacheRequest;
import org.saiditnet.redreader.common.TorCommon;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.util.List;

/**
 * HTTP Backend implementation using standard Java classes.
 *
 * Created by Mario Kosmiskas
 */
public class JavaHTTPBackend extends HTTPBackend {

    private static final String TAG = "JavaHTTPBackend";

	private List<PostField> postFields;

	@Override
	public void recreateHttpBackend() {}

	@Override
    public Request prepareRequest(Context context, RequestDetails details) {
        HttpURLConnection urlConn;
        try {
			if (TorCommon.isTorEnabled()) {
				Proxy tor = new Proxy(Proxy.Type.HTTP, new InetSocketAddress("127.0.0.1", 8118));
				urlConn = (HttpURLConnection) details.getUrl().toURL().openConnection(tor);
			} else {
				urlConn = (HttpURLConnection) details.getUrl().toURL().openConnection();
			}

			postFields = details.getPostFields();

            if (postFields != null) {
                urlConn.setDoOutput(true);
                urlConn.setRequestMethod("POST");
				urlConn.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");
            } else {
                urlConn.setRequestMethod("GET");
            }
        } catch (Exception e) {
			Log.e(TAG, "Error creating HTTP request for " + details.getUrl(), e);
            return null;
        }

        final HttpURLConnection conn = urlConn;
        return new Request() {
            @Override
            public void executeInThisThread(final Listener listener) {
                try {
                    try {
                        conn.connect();

						if (postFields != null) {
							OutputStream os = conn.getOutputStream();
							os.write(PostField.encodeList(postFields).getBytes());
						}
                    } catch(IOException e) {
                        listener.onError(CacheRequest.REQUEST_FAILURE_CONNECTION, e, null);
                        return;
                    }

                    final int status = conn.getResponseCode();
                    if (status == 200 || status == 202) {
                        final InputStream bodyStream = conn.getInputStream();
                        final Long bodyBytes = Long.valueOf(conn.getContentLength());
                        final String contentType = conn.getHeaderField("Content-Type");

                        listener.onSuccess(contentType, bodyBytes, bodyStream);
                    } else {
                        listener.onError(CacheRequest.REQUEST_FAILURE_REQUEST, null, status);
                    }
                } catch(Throwable t) {
                    listener.onError(CacheRequest.REQUEST_FAILURE_CONNECTION, t, null);
                }
            }

            @Override
            public void cancel() {
                conn.disconnect();
            }

            @Override
            public void addHeader(final String name, final String value) {
                conn.addRequestProperty(name, value);
            }
        };
    }
}
