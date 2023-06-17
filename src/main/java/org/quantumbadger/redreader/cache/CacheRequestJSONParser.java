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
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.GenericFactory;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

import java.io.IOException;
import java.io.InputStream;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;

public final class CacheRequestJSONParser implements CacheRequestCallbacks {

	private static final CachedThreadPool mThreadPool
			= new CachedThreadPool(5, "JSONParser");

	public interface Listener {

		void onJsonParsed(
				@NonNull JsonValue result,
				TimestampUTC timestamp,
				@NonNull UUID session,
				boolean fromCache);

		void onFailure(@NonNull RRError error);

		default void onDownloadNecessary() {
			// Do nothing by default
		}
	}

	@NonNull private final Context mContext;
	@NonNull private final Listener mListener;

	private final AtomicBoolean mNotifiedFailure = new AtomicBoolean(false);

	public CacheRequestJSONParser(
			@NonNull final Context context,
			@NonNull final Listener listener) {
		mContext = context;
		mListener = listener;
	}

	@Override
	public void onDataStreamAvailable(
			@NonNull final GenericFactory<SeekableInputStream, IOException> streamFactory,
			final TimestampUTC timestamp,
			@NonNull final UUID session,
			final boolean fromCache,
			@Nullable final String mimetype) {

		try {
			mThreadPool.add(() -> {
				final JsonValue jsonValue;

				try(InputStream is = streamFactory.create()) {
					jsonValue = JsonValue.parse(is);

				} catch(final IOException e) {
					if(!mNotifiedFailure.getAndSet(true)) {
						mListener.onFailure(General.getGeneralErrorForFailure(
								mContext,
								CacheRequest.REQUEST_FAILURE_PARSE,
								e,
								null,
								null,
								General.ignoreIOException(streamFactory)
										.filter(FailedRequestBody::from)));
					}
					return;
				}

				try {
					mListener.onJsonParsed(jsonValue, timestamp, session, fromCache);

				} catch(final Exception e) {
					BugReportActivity.handleGlobalError(mContext, e);
				}
			});

		} catch(final Exception e) {
			if(!mNotifiedFailure.getAndSet(true)) {
				onFailure(General.getGeneralErrorForFailure(
						mContext,
						CacheRequest.REQUEST_FAILURE_STORAGE,
						e,
						null,
						null,
						Optional.empty()));
			}
		}
	}

	@Override
	public void onFailure(@NonNull final RRError error) {
		if(!mNotifiedFailure.getAndSet(true)) {
			mListener.onFailure(error);
		}
	}
}
