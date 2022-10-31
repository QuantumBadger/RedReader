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

package org.saiditnet.redreader.reddit;

import android.content.Context;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.preference.PreferenceManager;
import android.support.annotation.UiThread;
import android.support.v7.app.AppCompatActivity;
import org.saiditnet.redreader.account.RedditAccount;
import org.saiditnet.redreader.activities.SessionChangeListener;
import org.saiditnet.redreader.cache.CacheManager;
import org.saiditnet.redreader.cache.CacheRequest;
import org.saiditnet.redreader.cache.downloadstrategy.DownloadStrategy;
import org.saiditnet.redreader.common.Constants;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.PrefsUtility;
import org.saiditnet.redreader.common.RRError;
import org.saiditnet.redreader.fragments.CommentListingFragment;
import org.saiditnet.redreader.jsonwrap.JsonBufferedArray;
import org.saiditnet.redreader.jsonwrap.JsonBufferedObject;
import org.saiditnet.redreader.jsonwrap.JsonValue;
import org.saiditnet.redreader.reddit.prepared.RedditChangeDataManager;
import org.saiditnet.redreader.reddit.prepared.RedditParsedComment;
import org.saiditnet.redreader.reddit.prepared.RedditParsedPost;
import org.saiditnet.redreader.reddit.prepared.RedditPreparedPost;
import org.saiditnet.redreader.reddit.prepared.RedditRenderableComment;
import org.saiditnet.redreader.reddit.things.RedditComment;
import org.saiditnet.redreader.reddit.things.RedditPost;
import org.saiditnet.redreader.reddit.things.RedditThing;
import org.saiditnet.redreader.reddit.url.RedditURLParser;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.UUID;

public class CommentListingRequest {

	private final Context mContext;
	private final CommentListingFragment mFragment;
	private final AppCompatActivity mActivity;
	private final RedditURLParser.RedditURL mCommentListingURL;

	private final boolean mParsePostSelfText;
	private final CacheManager mCacheManager;
	private final RedditURLParser.RedditURL mUrl;
	private final RedditAccount mUser;
	private final UUID mSession;
	private final DownloadStrategy mDownloadStrategy;

	private final Listener mListener;

	public CommentListingRequest(
			final Context context,
			final CommentListingFragment fragment,
			final AppCompatActivity activity, final RedditURLParser.RedditURL commentListingURL, final boolean parsePostSelfText,
			final RedditURLParser.RedditURL url,
			final RedditAccount user,
			final UUID session,
			final DownloadStrategy downloadStrategy,
			final Listener listener) {

		mContext = context;
		mFragment = fragment;
		mActivity = activity;
		mCommentListingURL = commentListingURL;
		mParsePostSelfText = parsePostSelfText;
		mUrl = url;
		mUser = user;
		mSession = session;
		mDownloadStrategy = downloadStrategy;
		mListener = listener;

		mCacheManager = CacheManager.getInstance(context);

		mCacheManager.makeRequest(new CommentListingCacheRequest());
	}

	private enum Event {
		EVENT_DOWNLOAD_NECESSARY,
		EVENT_DOWNLOAD_STARTED,
		EVENT_EXCEPTION,
		EVENT_FAILURE,
		EVENT_CACHED_COPY,
		EVENT_AUTHORIZING,
		EVENT_PARSE_START,
		EVENT_POST_DOWNLOADED,
		EVENT_ALL_ITEMS_DOWNLOADED
	}

	private static final Event[] EVENT_TYPES = Event.values();

	@UiThread
	public interface Listener {

		void onCommentListingRequestDownloadNecessary();

		void onCommentListingRequestDownloadStarted();

		void onCommentListingRequestException(Throwable t);

		void onCommentListingRequestFailure(RRError error);

		void onCommentListingRequestAuthorizing();

		void onCommentListingRequestCachedCopy(long timestamp);

		void onCommentListingRequestParseStart();

		void onCommentListingRequestPostDownloaded(RedditPreparedPost post);

		void onCommentListingRequestAllItemsDownloaded(ArrayList<RedditCommentListItem> items);
	}

	private final Handler mEventHandler = new Handler(Looper.getMainLooper()) {

		@Override
		public void handleMessage(final Message msg) {

			switch(EVENT_TYPES[msg.what]) {

				case EVENT_DOWNLOAD_NECESSARY:
					mListener.onCommentListingRequestDownloadNecessary();
					break;

				case EVENT_DOWNLOAD_STARTED:
					mListener.onCommentListingRequestDownloadStarted();
					break;

				case EVENT_EXCEPTION:
					mListener.onCommentListingRequestException((Throwable)msg.obj);
					break;

				case EVENT_FAILURE:
					mListener.onCommentListingRequestFailure((RRError)msg.obj);
					break;

				case EVENT_CACHED_COPY:
					mListener.onCommentListingRequestCachedCopy((Long)msg.obj);
					break;

				case EVENT_AUTHORIZING:
					mListener.onCommentListingRequestAuthorizing();
					break;

				case EVENT_PARSE_START:
					mListener.onCommentListingRequestParseStart();
					break;

				case EVENT_POST_DOWNLOADED:
					mListener.onCommentListingRequestPostDownloaded((RedditPreparedPost)msg.obj);
					break;

				case EVENT_ALL_ITEMS_DOWNLOADED:
					mListener.onCommentListingRequestAllItemsDownloaded((ArrayList<RedditCommentListItem>)msg.obj);
					break;

				default:
					throw new RuntimeException("Unknown event type");
			}
		}
	};

	private class CommentListingCacheRequest extends CacheRequest {

		protected CommentListingCacheRequest() {

			super(
					General.uriFromString(mUrl.generateJsonUri().toString()),
					mUser,
					mSession,
					Constants.Priority.API_COMMENT_LIST,
					0,
					mDownloadStrategy,
					Constants.FileType.COMMENT_LIST,
					DOWNLOAD_QUEUE_REDDIT_API,
					true,
					false,
					mContext);
		}

		@Override
		protected void onCallbackException(final Throwable t) {
			notifyListener(Event.EVENT_EXCEPTION, t);
		}

		@Override
		protected void onDownloadNecessary() {
			notifyListener(Event.EVENT_DOWNLOAD_NECESSARY);
		}

		@Override
		protected void onDownloadStarted() {
			notifyListener(Event.EVENT_DOWNLOAD_STARTED);
		}

		@Override
		protected void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {
			final RRError error = General.getGeneralErrorForFailure(context, type, t, status, url.toString());
			notifyListener(Event.EVENT_FAILURE, error);
		}

		@Override
		protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {
			if(authorizationInProgress) {
				notifyListener(Event.EVENT_AUTHORIZING);
			}
		}

		@Override
		protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {}

		@Override
		public void onJsonParseStarted(final JsonValue value, final long timestamp, final UUID session, final boolean fromCache) {

			String parentPostAuthor = null;

			if(mActivity instanceof SessionChangeListener) {
				((SessionChangeListener)mActivity).onSessionChanged(
						session,
						SessionChangeListener.SessionChangeType.COMMENTS,
						timestamp);
			}

			final Integer minimumCommentScore = PrefsUtility.pref_behaviour_comment_min(
					mContext,
					PreferenceManager.getDefaultSharedPreferences(context));

			if(fromCache) {
				notifyListener(Event.EVENT_CACHED_COPY, timestamp);
			}

			notifyListener(Event.EVENT_PARSE_START);

			try {
				// Download main post
				if(value.getType() == JsonValue.TYPE_ARRAY) {

					// lol, reddit api
					final JsonBufferedArray root = value.asArray();
					final JsonBufferedObject thing = root.get(0).asObject();
					final JsonBufferedObject listing = thing.getObject("data");
					final JsonBufferedArray postContainer = listing.getArray("children");
					final RedditThing postThing = postContainer.getObject(0, RedditThing.class);
					final RedditPost post = postThing.asPost();

					final RedditParsedPost parsedPost = new RedditParsedPost(post, mParsePostSelfText);

					final RedditPreparedPost preparedPost = new RedditPreparedPost(
							context,
							mCacheManager,
							0,
							parsedPost,
							timestamp,
							true,
							false);

					notifyListener(Event.EVENT_POST_DOWNLOADED, preparedPost);

					parentPostAuthor = parsedPost.getAuthor();
				}

				// Download comments

				final JsonBufferedObject thing;

				if(value.getType() == JsonValue.TYPE_ARRAY) {
					thing = value.asArray().get(1).asObject();
				} else {
					thing = value.asObject();
				}

				final JsonBufferedObject listing = thing.getObject("data");
				final JsonBufferedArray topLevelComments = listing.getArray("children");

				final ArrayList<RedditCommentListItem> items = new ArrayList<>(200);

				for(final JsonValue commentThingValue : topLevelComments) {
					buildCommentTree(commentThingValue, null, items, minimumCommentScore, parentPostAuthor);
				}

				final RedditChangeDataManager changeDataManager
						= RedditChangeDataManager.getInstance(mUser);

				for(final RedditCommentListItem item : items) {
					if(item.isComment()) {
						changeDataManager.update(timestamp, item.asComment().getParsedComment().getRawComment());
					}
				}

				notifyListener(Event.EVENT_ALL_ITEMS_DOWNLOADED, items);

			} catch (Throwable t) {
				notifyFailure(CacheRequest.REQUEST_FAILURE_PARSE, t, null, "Parse failure");
			}
		}
	}

	private void buildCommentTree(
			final JsonValue value,
			final RedditCommentListItem parent,
			final ArrayList<RedditCommentListItem> output,
			final Integer minimumCommentScore,
			final String parentPostAuthor)

			throws IOException, InterruptedException, IllegalAccessException, java.lang.InstantiationException,
			NoSuchMethodException, InvocationTargetException {

		final RedditThing thing = value.asObject(RedditThing.class);

		if(thing.getKind() == RedditThing.Kind.MORE_COMMENTS
				&& mUrl.pathType() == RedditURLParser.POST_COMMENT_LISTING_URL) {

			output.add(new RedditCommentListItem(
					thing.asMoreComments(),
					parent,
					mFragment,
					mActivity,
					mCommentListingURL));

		} else if(thing.getKind() == RedditThing.Kind.COMMENT) {

			final RedditComment comment = thing.asComment();
			final RedditCommentListItem item = new RedditCommentListItem(
					new RedditRenderableComment(
							new RedditParsedComment(comment),
							parentPostAuthor,
							minimumCommentScore,
							true),
					parent,
					mFragment,
					mActivity,
					mCommentListingURL);

			output.add(item);

			if(comment.replies.getType() == JsonValue.TYPE_OBJECT) {

				final JsonBufferedObject replies = comment.replies.asObject();
				final JsonBufferedArray children = replies.getObject("data").getArray("children");

				for(final JsonValue v : children) {
					buildCommentTree(v, item, output, minimumCommentScore, parentPostAuthor);
				}
			}
		}
	}

	private void notifyListener(Event eventType) {
		notifyListener(eventType, null);
	}

	private void notifyListener(Event eventType, Object object) {
		final Message message = Message.obtain();
		message.what = eventType.ordinal();
		message.obj = object;
		mEventHandler.sendMessage(message);
	}
}
