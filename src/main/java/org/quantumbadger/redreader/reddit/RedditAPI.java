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

package org.quantumbadger.redreader.reddit;

import android.content.Context;
import android.support.annotation.IntDef;

import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.TimestampBound;
import org.quantumbadger.redreader.http.HTTPBackend;
import org.quantumbadger.redreader.io.RequestResponseHandler;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.reddit.api.SubredditRequestFailure;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.things.RedditThing;
import org.quantumbadger.redreader.reddit.things.RedditUser;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.net.URI;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.quantumbadger.redreader.http.HTTPBackend.PostField;

public final class RedditAPI {

	public static final int ACTION_UPVOTE = 0;
	public static final int ACTION_UNVOTE = 1;
	public static final int ACTION_DOWNVOTE = 2;
	public static final int ACTION_SAVE = 3;
	public static final int ACTION_HIDE = 4;
	public static final int ACTION_UNSAVE = 5;
	public static final int ACTION_UNHIDE = 6;
	public static final int ACTION_REPORT = 7;
	public static final int ACTION_DELETE = 8;

	public static final int SUBSCRIPTION_ACTION_SUBSCRIBE = 0;
	public static final int SUBSCRIPTION_ACTION_UNSUBSCRIBE = 1;

	@IntDef({ACTION_UPVOTE, ACTION_UNVOTE, ACTION_DOWNVOTE, ACTION_SAVE, ACTION_HIDE, ACTION_UNSAVE,
		ACTION_UNHIDE, ACTION_REPORT, ACTION_DELETE})
	@Retention(RetentionPolicy.SOURCE)
	public @interface RedditAction {}

	@IntDef({SUBSCRIPTION_ACTION_SUBSCRIBE, SUBSCRIPTION_ACTION_UNSUBSCRIBE})
	@Retention(RetentionPolicy.SOURCE)
	public @interface RedditSubredditAction {}

	public static void submit(final CacheManager cm,
							   final APIResponseHandler.ActionResponseHandler responseHandler,
							   final RedditAccount user,
							   final boolean is_self,
							   final String subreddit,
							   final String title,
							   final String body,
							   final String captchaId,
							   final String captchaText,
							   final Context context) {

		final LinkedList<PostField> postFields = new LinkedList<>();
		postFields.add(new PostField("kind", is_self ? "self" : "link"));
		postFields.add(new PostField("sendreplies", "true"));
		postFields.add(new PostField("sr", subreddit));
		postFields.add(new PostField("title", title));
		postFields.add(new PostField("captcha", captchaText));
		postFields.add(new PostField("iden", captchaId));

		if(is_self)
			postFields.add(new PostField("text", body));
		else
			postFields.add(new PostField("url", body));


		cm.makeRequest(new APIPostRequest(Constants.Reddit.getUri("/api/submit"), user, postFields, context) {

			@Override
			public void onJsonParseStarted(JsonValue result, long timestamp, UUID session, boolean fromCache) {

				System.out.println(result.toString());

				try {
					final APIResponseHandler.APIFailureType failureType = findFailureType(result);

					if(failureType != null) {
						responseHandler.notifyFailure(failureType);
						return;
					}

				} catch(Throwable t) {
					notifyFailure(CacheRequest.REQUEST_FAILURE_PARSE, t, null, "JSON failed to parse");
				}

				responseHandler.notifySuccess();
			}

			@Override
			protected void onCallbackException(Throwable t) {
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onFailure(@CacheRequest.RequestFailureType int type, Throwable t, Integer status, String readableMessage) {
				responseHandler.notifyFailure(type, t, status, readableMessage);
			}
		});
	}

	public static void comment(final CacheManager cm,
							   final APIResponseHandler.ActionResponseHandler responseHandler,
							   final RedditAccount user,
							   final String parentIdAndType,
							   final String markdown,
							   final Context context) {

		final LinkedList<PostField> postFields = new LinkedList<>();
		postFields.add(new PostField("thing_id", parentIdAndType));
		postFields.add(new PostField("text", markdown));

		cm.makeRequest(new APIPostRequest(Constants.Reddit.getUri("/api/comment"), user, postFields, context) {

			@Override
			public void onJsonParseStarted(JsonValue result, long timestamp, UUID session, boolean fromCache) {

				try {
					final APIResponseHandler.APIFailureType failureType = findFailureType(result);

					if(failureType != null) {
						responseHandler.notifyFailure(failureType);
						return;
					}

				} catch(Throwable t) {
					notifyFailure(CacheRequest.REQUEST_FAILURE_PARSE, t, null, "JSON failed to parse");
				}

				responseHandler.notifySuccess();
			}

			@Override
			protected void onCallbackException(Throwable t) {
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onFailure(@CacheRequest.RequestFailureType int type, Throwable t, Integer status, String readableMessage) {
				responseHandler.notifyFailure(type, t, status, readableMessage);
			}
		});
	}

	public static void markAllAsRead(
			final CacheManager cm,
			final APIResponseHandler.ActionResponseHandler responseHandler,
			final RedditAccount user,
			final Context context) {

		final LinkedList<PostField> postFields = new LinkedList<>();

		cm.makeRequest(new APIPostRequest(Constants.Reddit.getUri("/api/read_all_messages"), user, postFields, context) {

			@Override
			public void onJsonParseStarted(JsonValue result, long timestamp, UUID session, boolean fromCache) {

				try {
					final APIResponseHandler.APIFailureType failureType = findFailureType(result);

					if(failureType != null) {
						responseHandler.notifyFailure(failureType);
						return;
					}

				} catch(Throwable t) {
					notifyFailure(CacheRequest.REQUEST_FAILURE_PARSE, t, null, "JSON failed to parse");
				}

				responseHandler.notifySuccess();
			}

			@Override
			protected void onCallbackException(Throwable t) {
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onFailure(@CacheRequest.RequestFailureType int type, Throwable t, Integer status, String readableMessage) {
				responseHandler.notifyFailure(type, t, status, readableMessage);
			}
		});
	}

	public static void editComment(final CacheManager cm,
								   final APIResponseHandler.ActionResponseHandler responseHandler,
								   final RedditAccount user,
								   final String commentIdAndType,
								   final String markdown,
								   final Context context) {

		final LinkedList<PostField> postFields = new LinkedList<>();
		postFields.add(new PostField("thing_id", commentIdAndType));
		postFields.add(new PostField("text", markdown));

		cm.makeRequest(new APIPostRequest(Constants.Reddit.getUri("/api/editusertext"), user, postFields, context) {

			@Override
			public void onJsonParseStarted(JsonValue result, long timestamp, UUID session, boolean fromCache) {

				try {
					final APIResponseHandler.APIFailureType failureType = findFailureType(result);

					if(failureType != null) {
						responseHandler.notifyFailure(failureType);
						return;
					}

				} catch(Throwable t) {
					notifyFailure(CacheRequest.REQUEST_FAILURE_PARSE, t, null, "JSON failed to parse");
				}

				responseHandler.notifySuccess();
			}

			@Override
			protected void onCallbackException(Throwable t) {
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onFailure(@CacheRequest.RequestFailureType int type, Throwable t, Integer status, String readableMessage) {
				responseHandler.notifyFailure(type, t, status, readableMessage);
			}
		});
	}

	public static void newCaptcha(final CacheManager cm,
								   final APIResponseHandler.NewCaptchaResponseHandler responseHandler,
								   final RedditAccount user,
								   final Context context) {

		final LinkedList<PostField> postFields = new LinkedList<>();

		cm.makeRequest(new APIPostRequest(Constants.Reddit.getUri("/api/new_captcha"), user, postFields, context) {

			@Override
			public void onJsonParseStarted(JsonValue result, long timestamp, UUID session, boolean fromCache) {

				try {
					final APIResponseHandler.APIFailureType failureType = findFailureType(result);

					if(failureType != null) {
						responseHandler.notifyFailure(failureType);
						return;
					}

				} catch(Throwable t) {
					notifyFailure(CacheRequest.REQUEST_FAILURE_PARSE, t, null, "JSON failed to parse");
				}

				responseHandler.notifySuccess(findCaptchaId(result));
			}

			@Override
			protected void onCallbackException(Throwable t) {
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onFailure(@CacheRequest.RequestFailureType int type, Throwable t, Integer status, String readableMessage) {
				responseHandler.notifyFailure(type, t, status, readableMessage);
			}
		});
	}

	public static void action(final CacheManager cm,
							  final APIResponseHandler.ActionResponseHandler responseHandler,
							  final RedditAccount user,
							  final String idAndType,
							  final @RedditAction int action,
							  final Context context) {

		final LinkedList<PostField> postFields = new LinkedList<>();
		postFields.add(new PostField("id", idAndType));

		final URI url = prepareActionUri(action, postFields);

		cm.makeRequest(new APIPostRequest(url, user, postFields, context) {
			@Override
			protected void onCallbackException(final Throwable t) {
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {
				responseHandler.notifyFailure(type, t, status, readableMessage);
			}

			@Override
			public void onJsonParseStarted(final JsonValue result, final long timestamp, final UUID session, final boolean fromCache) {

				try {

					final APIResponseHandler.APIFailureType failureType = findFailureType(result);

					if(failureType != null) {
						responseHandler.notifyFailure(failureType);
						return;
					}

				} catch(Throwable t) {
					notifyFailure(CacheRequest.REQUEST_FAILURE_PARSE, t, null, "JSON failed to parse");
				}

				responseHandler.notifySuccess();
			}
		});
	}

	private static URI prepareActionUri(final @RedditAction int action, final LinkedList<PostField> postFields) {
		switch(action) {
			case ACTION_DOWNVOTE:
				postFields.add(new PostField("dir", "-1"));
				return Constants.Reddit.getUri(Constants.Reddit.PATH_VOTE);

			case ACTION_UNVOTE:
				postFields.add(new PostField("dir", "0"));
				return Constants.Reddit.getUri(Constants.Reddit.PATH_VOTE);

			case ACTION_UPVOTE:
				postFields.add(new PostField("dir", "1"));
				return Constants.Reddit.getUri(Constants.Reddit.PATH_VOTE);

			case ACTION_SAVE: return Constants.Reddit.getUri(Constants.Reddit.PATH_SAVE);
			case ACTION_HIDE: return Constants.Reddit.getUri(Constants.Reddit.PATH_HIDE);
			case ACTION_UNSAVE: return Constants.Reddit.getUri(Constants.Reddit.PATH_UNSAVE);
			case ACTION_UNHIDE: return Constants.Reddit.getUri(Constants.Reddit.PATH_UNHIDE);
			case ACTION_REPORT: return Constants.Reddit.getUri(Constants.Reddit.PATH_REPORT);
			case ACTION_DELETE: return Constants.Reddit.getUri(Constants.Reddit.PATH_DELETE);

			default:
				throw new RuntimeException("Unknown post/comment action");
		}
	}

	public static void subscriptionAction(final CacheManager cm,
							  final APIResponseHandler.ActionResponseHandler responseHandler,
							  final RedditAccount user,
							  final String subredditCanonicalName,
							  final @RedditSubredditAction int action,
							  final Context context) {

		RedditSubredditManager.getInstance(context, user).getSubreddit(
				subredditCanonicalName,
				TimestampBound.ANY,
				new RequestResponseHandler<RedditSubreddit, SubredditRequestFailure>() {

					@Override
					public void onRequestFailed(SubredditRequestFailure failureReason) {
						responseHandler.notifyFailure(
								failureReason.requestFailureType,
								failureReason.t,
								failureReason.statusLine,
								failureReason.readableMessage);

					}

					@Override
					public void onRequestSuccess(RedditSubreddit subreddit, long timeCached) {

						final LinkedList<PostField> postFields = new LinkedList<>();

						postFields.add(new PostField("sr", subreddit.name));

						final URI url = subscriptionPrepareActionUri(action, postFields);

						cm.makeRequest(new APIPostRequest(url, user, postFields, context) {
							@Override
							protected void onCallbackException(final Throwable t) {
								BugReportActivity.handleGlobalError(context, t);
							}

							@Override
							protected void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {
								responseHandler.notifyFailure(type, t, status, readableMessage);
							}

							@Override
							public void onJsonParseStarted(final JsonValue result, final long timestamp, final UUID session, final boolean fromCache) {

								try {

									final APIResponseHandler.APIFailureType failureType = findFailureType(result);

									if(failureType != null) {
										responseHandler.notifyFailure(failureType);
										return;
									}

								} catch(Throwable t) {
									notifyFailure(CacheRequest.REQUEST_FAILURE_PARSE, t, null, "JSON failed to parse");
								}

								responseHandler.notifySuccess();
							}
						});
					}
				},
				null
		);
	}

	private static URI subscriptionPrepareActionUri(final @RedditSubredditAction int action,
										final LinkedList<PostField> postFields) {
		switch(action) {
			case SUBSCRIPTION_ACTION_SUBSCRIBE:
				postFields.add(new PostField("action", "sub"));
				return Constants.Reddit.getUri(Constants.Reddit.PATH_SUBSCRIBE);

			case SUBSCRIPTION_ACTION_UNSUBSCRIBE:
				postFields.add(new PostField("action", "unsub"));
				return Constants.Reddit.getUri(Constants.Reddit.PATH_SUBSCRIBE);

			default:
				throw new RuntimeException("Unknown subreddit action");
		}
	}

	public static void getUser(final CacheManager cm,
							   final String usernameToGet,
							   final APIResponseHandler.UserResponseHandler responseHandler,
							   final RedditAccount user,
							   final @CacheRequest.DownloadType int downloadType,
							   final boolean cancelExisting,
							   final Context context) {

		final URI uri = Constants.Reddit.getUri("/user/" + usernameToGet + "/about.json");

		cm.makeRequest(new APIGetRequest(uri, user, Constants.Priority.API_USER_ABOUT, Constants.FileType.USER_ABOUT, downloadType, true, cancelExisting, context) {

			@Override
			protected void onDownloadNecessary() {}

			@Override
			protected void onDownloadStarted() {
				responseHandler.notifyDownloadStarted();
			}

			@Override
			protected void onCallbackException(final Throwable t) {
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {
				responseHandler.notifyFailure(type, t, status, readableMessage);
			}

			@Override
			public void onJsonParseStarted(final JsonValue result, final long timestamp, final UUID session, final boolean fromCache) {

				try {

					final RedditThing userThing = result.asObject(RedditThing.class);
					final RedditUser userResult = userThing.asUser();
					responseHandler.notifySuccess(userResult, timestamp);

				} catch(Throwable t) {
					// TODO look for error
					notifyFailure(CacheRequest.REQUEST_FAILURE_PARSE, t, null, "JSON parse failed for unknown reason");
				}
			}
		});
	}

	// lol, reddit api
	private static APIResponseHandler.APIFailureType findFailureType(final JsonValue response) {

		// TODO doesn't handle unknown failures

		// TODO handle 403 forbidden

		switch(response.getType()) {

			case JsonValue.TYPE_OBJECT:

				for(final Map.Entry<String, JsonValue> v : response.asObject()) {
					final APIResponseHandler.APIFailureType failureType = findFailureType(v.getValue());
					if(failureType != null) return failureType;
				}

				break;

			case JsonValue.TYPE_ARRAY:

				for(final JsonValue v : response.asArray()) {
					final APIResponseHandler.APIFailureType failureType = findFailureType(v);
					if(failureType != null) return failureType;
				}

				break;

			case JsonValue.TYPE_STRING:

				if(Constants.Reddit.isApiErrorUser(response.asString()))
					return APIResponseHandler.APIFailureType.INVALID_USER;

				if(Constants.Reddit.isApiErrorCaptcha(response.asString()))
					return APIResponseHandler.APIFailureType.BAD_CAPTCHA;

				if(Constants.Reddit.isApiErrorNotAllowed(response.asString()))
					return APIResponseHandler.APIFailureType.NOTALLOWED;

				if(Constants.Reddit.isApiErrorSubredditRequired(response.asString()))
					return APIResponseHandler.APIFailureType.SUBREDDIT_REQUIRED;

				if(Constants.Reddit.isApiErrorURLRequired(response.asString()))
					return APIResponseHandler.APIFailureType.URL_REQUIRED;

				if(Constants.Reddit.isApiTooFast(response.asString()))
					return APIResponseHandler.APIFailureType.TOO_FAST;

				break;

			default:
				// Ignore
		}

		return null;
	}

	// lol, reddit api
	private static String findCaptchaId(final JsonValue response) {

		switch(response.getType()) {

			case JsonValue.TYPE_OBJECT:

				for(final Map.Entry<String, JsonValue> v : response.asObject()) {
					final String captchaId = findCaptchaId(v.getValue());
					if(captchaId != null) return captchaId;
				}

				break;

			case JsonValue.TYPE_ARRAY:

				for(final JsonValue v : response.asArray()) {
					final String captchaId = findCaptchaId(v);
					if(captchaId != null) return captchaId;
				}

				break;

			case JsonValue.TYPE_STRING:

				if(response.asString().length() > 20) { // This is probably it :S
					return response.asString();
				}

				break;

			default:
				// Ignore
		}

		return null;
	}

	private static abstract class APIPostRequest extends CacheRequest {

		@Override
		protected void onDownloadNecessary() {}

		@Override
		protected void onDownloadStarted() {}

		public APIPostRequest(final URI url, final RedditAccount user, final List<HTTPBackend.PostField> postFields, final Context context) {
			super(url, user, null, Constants.Priority.API_ACTION, 0, DOWNLOAD_FORCE, Constants.FileType.NOCACHE, DOWNLOAD_QUEUE_REDDIT_API, true, postFields, false, false, context);
		}

		@Override
		protected final void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {
			throw new RuntimeException("onSuccess called for uncached request");
		}

		@Override
		protected final void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {
		}

		@Override
		public abstract void onJsonParseStarted(JsonValue result, long timestamp, UUID session, boolean fromCache);
	}

	// TODO merge get and post into one?
	private static abstract class APIGetRequest extends CacheRequest {

		public APIGetRequest(final URI url, final RedditAccount user, final int priority, final int fileType, final @DownloadType int downloadType, final boolean cache, final boolean cancelExisting, final Context context) {
			super(url, user, null, priority, 0, downloadType, fileType, DOWNLOAD_QUEUE_REDDIT_API, true, null, cache, cancelExisting, context);
		}

		@Override
		protected final void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {
			if(!cache) throw new RuntimeException("onSuccess called for uncached request");
		}

		@Override
		protected final void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {}

		@Override
		public abstract void onJsonParseStarted(JsonValue result, long timestamp, UUID session, boolean fromCache);
	}
}
