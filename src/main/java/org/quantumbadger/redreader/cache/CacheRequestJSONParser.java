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

import android.content.Context;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.common.CachedThreadPool;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.UUID;

public final class CacheRequestJSONParser
		implements CacheRequestCallbacks, CacheDataStreamChunkConsumer {

	private static final CachedThreadPool mThreadPool
			= new CachedThreadPool(5, "JSONParser");

	public interface Listener {

		void onJsonParsed(
				@NonNull JsonValue result,
				long timestamp,
				@NonNull UUID session,
				boolean fromCache);

		void onFailure(
				int type,
				@Nullable Throwable t,
				@Nullable Integer httpStatus,
				@Nullable String readableMessage);

		default void onDownloadNecessary() {
			// Do nothing by default
		}
	}

	@NonNull private final Context mContext;
	@NonNull private final Listener mListener;
	private final ByteArrayOutputStream mBuffer = new ByteArrayOutputStream(64 * 1024);

	public CacheRequestJSONParser(
			@NonNull final Context context,
			@NonNull final Listener listener) {
		mContext = context;
		mListener = listener;
	}

	@NonNull
	@Override
	public CacheDataStreamChunkConsumer onDataStreamAvailable() {
		return this;
	}

	@Override
	public void onFailure(
			final int type,
			@Nullable final Throwable t,
			@Nullable final Integer httpStatus,
			@Nullable final String readableMessage) {

		mListener.onFailure(type, t, httpStatus, readableMessage);
	}

	@Override
	public void onDataStreamChunk(
			@NonNull final byte[] dataReused,
			final int offset,
			final int length) {

		mBuffer.write(dataReused, offset, length);
	}

	@Override
	public void onDataStreamSuccess() {
		// Nothing to do
	}

	@Override
	public void onDataStreamCancel() {
		// Nothing to do
	}

	@Override
	public void onSuccess(
			@NonNull final CacheManager.ReadableCacheFile cacheFile,
			final long timestamp,
			@NonNull final UUID session,
			final boolean fromCache,
			@Nullable final String mimetype) {

		try {
			mThreadPool.add(() -> {
				final JsonValue jsonValue;

				try {
					jsonValue = new JsonValue(mBuffer.toByteArray());

				} catch(final IOException e) {
					mListener.onFailure(
							CacheRequest.REQUEST_FAILURE_PARSE,
							e,
							null,
							"Exception during JSON parse");
					return;
				}

				try {
					mListener.onJsonParsed(jsonValue, timestamp, session, fromCache);

				} catch(final Exception e) {
					BugReportActivity.handleGlobalError(mContext, e);
				}
			});

		} catch(final Exception e) {
			onFailure(
					CacheRequest.REQUEST_FAILURE_STORAGE,
					e,
					null,
					"Exception in CacheRequestJSONCallbacks");
		}
	}
}
