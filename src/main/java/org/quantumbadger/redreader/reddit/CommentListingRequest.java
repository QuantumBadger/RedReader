/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.quantumbadger.redreader.reddit;

import android.content.Context;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import org.apache.http.StatusLine;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.*;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedArray;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedComment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedMoreComments;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.things.RedditComment;
import org.quantumbadger.redreader.reddit.things.RedditMoreComments;
import org.quantumbadger.redreader.reddit.things.RedditPost;
import org.quantumbadger.redreader.reddit.things.RedditThing;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.UUID;

public class CommentListingRequest {

	public CommentListingRequest(
			final Context context,
			final EnumSet<PrefsUtility.AppearanceCommentHeaderItems> commentHeaderItems,
			final boolean parsePostSelfText,
			final RedditURLParser.RedditURL url,
			final RedditAccount user,
			final UUID session,
			final CacheRequest.DownloadType downloadType,
			final Listener listener) {

		mContext = context;
		mCommentHeaderItems = commentHeaderItems;
		mParsePostSelfText = parsePostSelfText;
		mUrl = url;
		mUser = user;
		mSession = session;
		mDownloadType = downloadType;
		mListener = listener;

		mCacheManager = CacheManager.getInstance(context);

		mCacheManager.makeRequest(new CommentListingCacheRequest());
	}

	private static enum Event {
		EVENT_DOWNLOAD_NECESSARY,
		EVENT_DOWNLOAD_STARTED,
		EVENT_EXCEPTION,
		EVENT_FAILURE,
		EVENT_CACHED_COPY,
		EVENT_AUTHORIZING,
		EVENT_PARSE_START,
		EVENT_POST_DOWNLOADED,
		EVENT_ITEM_DOWNLOADED,
		EVENT_COMPLETE
	}

	private static final Event[] EVENT_TYPES = Event.values();

	public interface Listener {

		// All called from UI thread
		void onCommentListingRequestDownloadNecessary();
		void onCommentListingRequestDownloadStarted();
		void onCommentListingRequestException(Throwable t);
		void onCommentListingRequestFailure(RRError error);
		void onCommentListingRequestAuthorizing();
		void onCommentListingRequestCachedCopy(long timestamp);
		void onCommentListingRequestParseStart();
		void onCommentListingRequestPostDownloaded(RedditPreparedPost post);
		void onCommentListingRequestItemDownloaded(RedditCommentListItem item);
		void onCommentListingRequestComplete();
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

				case EVENT_ITEM_DOWNLOADED:
					mListener.onCommentListingRequestItemDownloaded((RedditCommentListItem)msg.obj);
					break;

				case EVENT_COMPLETE:
					mListener.onCommentListingRequestComplete();
					break;

				default:
					throw new RuntimeException("Unknown event type");
			}
		}
	};

	private final Context mContext;
	private final EnumSet<PrefsUtility.AppearanceCommentHeaderItems> mCommentHeaderItems;
	private final boolean mParsePostSelfText;
	private final CacheManager mCacheManager;
	private final RedditURLParser.RedditURL mUrl;
	private final RedditAccount mUser;
	private final UUID mSession;
	private final CacheRequest.DownloadType mDownloadType;

	private final Listener mListener;


	private RedditPreparedPost mParentPost = null;

	private class CommentListingCacheRequest extends CacheRequest {

		protected CommentListingCacheRequest() {

			super(
					General.uriFromString(mUrl.generateJsonUri().toString()),
					mUser,
					mSession,
					Constants.Priority.API_COMMENT_LIST,
					0,
					mDownloadType,
					Constants.FileType.COMMENT_LIST,
					true,
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
		protected void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {
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

			if(fromCache) {
				notifyListener(Event.EVENT_CACHED_COPY, timestamp);
			}

			notifyListener(Event.EVENT_PARSE_START);

			try {
				// Download main post
				if(value.getType() == JsonValue.Type.ARRAY) {

					// lol, reddit api
					final JsonBufferedArray root = value.asArray();
					final JsonBufferedObject thing = root.get(0).asObject();
					final JsonBufferedObject listing = thing.getObject("data");
					final JsonBufferedArray postContainer = listing.getArray("children");
					final RedditThing postThing = postContainer.getObject(0, RedditThing.class);
					final RedditPost post = postThing.asPost();

					final RedditPreparedPost preparedPost = new RedditPreparedPost(
							context,
							mCacheManager,
							0,
							post,
							timestamp,
							true,
							false,
							false,
							false,
							user,
							mParsePostSelfText);

					notifyListener(Event.EVENT_POST_DOWNLOADED, preparedPost);

					mParentPost = preparedPost;
				}

				// Download comments

				final JsonBufferedObject thing;

				if(value.getType() == JsonValue.Type.ARRAY) {
					thing = value.asArray().get(1).asObject();
				} else {
					thing = value.asObject();
				}

				final JsonBufferedObject listing = thing.getObject("data");
				final JsonBufferedArray topLevelComments = listing.getArray("children");

				final String parentId;

				switch(mUrl.pathType()) {
					case PostCommentListingURL:
						parentId = "t3_" + mUrl.asPostCommentListURL().postId;
						break;
					case UserCommentListingURL:
						parentId = "/u/" + mUrl.asUserCommentListURL().user + "/comments";
						break;
					default:
						throw new RuntimeException("Unknown url type");
				}

				final HashSet<String> needsChanging = RedditChangeDataManager
						.getInstance(context)
						.getChangedForParent(parentId, user);

				for(final JsonValue commentThingValue : topLevelComments) {
					buildComments(commentThingValue, null, timestamp, needsChanging);
				}

				notifyListener(Event.EVENT_COMPLETE);


			} catch (Throwable t) {
				notifyFailure(RequestFailureType.PARSE, t, null, "Parse failure");
			}
		}
	}

	private void buildComments(final JsonValue value, final RedditCommentListItem parent, final long timestamp, final HashSet<String> needsChanging) throws IOException, InterruptedException, IllegalAccessException, java.lang.InstantiationException, NoSuchMethodException, InvocationTargetException {

		final RedditThing commentThing = value.asObject(RedditThing.class);

		final RedditCommentListItem item;
		boolean shouldRecurse = false;

		if(commentThing.getKind() == RedditThing.Kind.MORE_COMMENTS
				&& mUrl.pathType() == RedditURLParser.PathType.PostCommentListingURL) {

			final RedditMoreComments redditMoreComments = commentThing.asMoreComments();
			final RedditPreparedMoreComments preparedMoreComments = new RedditPreparedMoreComments(redditMoreComments, mUrl.asPostCommentListURL());
			item = new RedditCommentListItem(parent, preparedMoreComments);

		} else if(commentThing.getKind() == RedditThing.Kind.COMMENT) {

			final RedditComment comment = commentThing.asComment();
			final RedditPreparedComment preparedComment = new RedditPreparedComment(
					mContext,
					comment,
					timestamp,
					needsChanging.contains(comment.name),
					mParentPost,
					mUser,
					mCommentHeaderItems);

			if(parent != null && parent.isComment()) {
				parent.asComment().addChild(preparedComment);
			}

			item = new RedditCommentListItem(parent, preparedComment);

			if(comment.replies.getType() == JsonValue.Type.OBJECT) {
				shouldRecurse = true;
			}

		} else {
			return;
		}

		notifyListener(Event.EVENT_ITEM_DOWNLOADED, item);

		if(shouldRecurse) {
			final RedditComment comment = commentThing.asComment();
			final JsonBufferedObject replies = comment.replies.asObject();
			final JsonBufferedArray children = replies.getObject("data").getArray("children");

			for(final JsonValue v : children) {
				buildComments(v, item, timestamp, needsChanging);
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
