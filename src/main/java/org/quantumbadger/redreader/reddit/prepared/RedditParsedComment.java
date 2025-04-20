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

package org.quantumbadger.redreader.reddit.prepared;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.text.style.ImageSpan;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;

import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.CacheRequestCallbacks;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.quantumbadger.redreader.common.BetterSSB;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.GenericFactory;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.reddit.kthings.MaybeParseError;
import org.quantumbadger.redreader.reddit.kthings.RedditComment;
import org.quantumbadger.redreader.reddit.kthings.RedditIdAndType;
import org.quantumbadger.redreader.reddit.kthings.UrlEncodedString;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.html.HtmlReader;
import org.quantumbadger.redreader.reddit.things.RedditThingWithIdAndType;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.UUID;

public class RedditParsedComment implements RedditThingWithIdAndType {

	private final RedditComment mSrc;

	@NonNull private final BodyElement mBody;

	private final BetterSSB mFlair;

	public RedditParsedComment(
			final RedditComment comment,
			final AppCompatActivity activity) {

		mSrc = comment;

		mBody = HtmlReader.parse(
				comment.getBody_html().getDecoded(), // TODO nullable?
				activity);

		final String flair = General.mapIfNotNull(
				comment.getAuthor_flair_text(),
				UrlEncodedString::getDecoded);

		if(flair != null) {
			mFlair = new BetterSSB();
			mFlair.append(flair);

			if (comment.getAuthor_flair_richtext() != null) {
				getFlairEmotes(comment.getAuthor_flair_richtext(), activity);
			}
		} else {
			mFlair = null;
		}
	}

	@NonNull
	public BodyElement getBody() {
		return mBody;
	}

	public BetterSSB getFlair() {
		return mFlair;
	}

	@Override
	public String getIdAlone() {
		return mSrc.getIdAlone();
	}

	@Override
	public RedditIdAndType getIdAndType() {
		return mSrc.getIdAndType();
	}

	public RedditComment getRawComment() {
		return mSrc;
	}

	private void getFlairEmotes(
			final List<MaybeParseError<RedditComment.FlairEmoteData>> flairRichtext,
			final AppCompatActivity activity) {
		for (final MaybeParseError<RedditComment.FlairEmoteData> flairEmoteData : flairRichtext) {
			if (!(flairEmoteData instanceof MaybeParseError.Ok)) {
				continue;
			}

			final RedditComment.FlairEmoteData flairEmoteObject =
					((MaybeParseError.Ok< RedditComment.FlairEmoteData >) flairEmoteData)
							.getValue();

			final String objectType = flairEmoteObject.getE();

			if (objectType != null && objectType.equals("emoji")) {
				final String placeholder = flairEmoteObject.getA();
				final String url = flairEmoteObject.getU();

				CacheManager.getInstance(activity).makeRequest(new CacheRequest(
						General.uriFromString(url),
						RedditAccountManager.getAnon(),
						null,
						new Priority(Constants.Priority.API_COMMENT_LIST),
						DownloadStrategyIfNotCached.INSTANCE,
						Constants.FileType.IMAGE,
						CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE,
						activity,
						new CacheRequestCallbacks() {
							Bitmap image = null;

							@Override
							public void onDataStreamComplete(
									@NonNull final GenericFactory<SeekableInputStream, IOException>
											stream,
									final long timestamp,
									@NonNull final UUID session,
									final boolean fromCache,
									@Nullable final String mimetype) {
								try (InputStream is = stream.create()) {
									image = BitmapFactory.decodeStream(is);

									image = Bitmap.createScaledBitmap(image,
											image.getWidth() / 2,
											image.getHeight() / 2,
											true);

									if (image == null) {
										throw new IOException("Failed to decode bitmap");
									}

									final ImageSpan span = new ImageSpan(
											activity.getApplicationContext(),
											image);

									mFlair.replace(placeholder, span);
								} catch (final Throwable t) {
									onFailure(
											CacheRequest.REQUEST_FAILURE_CONNECTION,
											t,
											null,
											"Exception while downloading emote",
											Optional.empty());
								}
							}

							@Override
							public void onFailure(
										final int type,
										@Nullable final Throwable t,
										@Nullable final Integer httpStatus,
										@Nullable final String readableMessage,
										@NonNull final Optional<FailedRequestBody> body) {

							}
						}
				));
			}
		}
	}
}
