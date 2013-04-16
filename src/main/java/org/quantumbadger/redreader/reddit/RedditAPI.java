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
import org.apache.http.NameValuePair;
import org.apache.http.StatusLine;
import org.apache.http.message.BasicNameValuePair;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedArray;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.things.RedditThing;
import org.quantumbadger.redreader.reddit.things.RedditUser;

import java.net.URI;
import java.util.*;

public final class RedditAPI {

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

		final LinkedList<NameValuePair> postFields = new LinkedList<NameValuePair>();
		postFields.add(new BasicNameValuePair("kind", is_self ? "self" : "link"));
		postFields.add(new BasicNameValuePair("sendreplies", "true"));
		postFields.add(new BasicNameValuePair("uh", user.modhash));
		postFields.add(new BasicNameValuePair("sr", subreddit));
		postFields.add(new BasicNameValuePair("title", title));
		postFields.add(new BasicNameValuePair("captcha", captchaText));
		postFields.add(new BasicNameValuePair("iden", captchaId));

		if(is_self)
			postFields.add(new BasicNameValuePair("text", body));
		else
			postFields.add(new BasicNameValuePair("url", body));


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
					notifyFailure(RequestFailureType.PARSE, t, null, "JSON failed to parse");
				}

				responseHandler.notifySuccess();
			}

			@Override
			protected void onCallbackException(Throwable t) {
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onFailure(RequestFailureType type, Throwable t, StatusLine status, String readableMessage) {
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

		final LinkedList<NameValuePair> postFields = new LinkedList<NameValuePair>();
		postFields.add(new BasicNameValuePair("thing_id", parentIdAndType));
		postFields.add(new BasicNameValuePair("uh", user.modhash));
		postFields.add(new BasicNameValuePair("text", markdown));

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
					notifyFailure(RequestFailureType.PARSE, t, null, "JSON failed to parse");
				}

				responseHandler.notifySuccess();
			}

			@Override
			protected void onCallbackException(Throwable t) {
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onFailure(RequestFailureType type, Throwable t, StatusLine status, String readableMessage) {
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

		final LinkedList<NameValuePair> postFields = new LinkedList<NameValuePair>();
		postFields.add(new BasicNameValuePair("thing_id", commentIdAndType));
		postFields.add(new BasicNameValuePair("uh", user.modhash));
		postFields.add(new BasicNameValuePair("text", markdown));

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
					notifyFailure(RequestFailureType.PARSE, t, null, "JSON failed to parse");
				}

				responseHandler.notifySuccess();
			}

			@Override
			protected void onCallbackException(Throwable t) {
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onFailure(RequestFailureType type, Throwable t, StatusLine status, String readableMessage) {
				responseHandler.notifyFailure(type, t, status, readableMessage);
			}
		});
	}

	public static void newCaptcha(final CacheManager cm,
								   final APIResponseHandler.NewCaptchaResponseHandler responseHandler,
								   final RedditAccount user,
								   final Context context) {

		final LinkedList<NameValuePair> postFields = new LinkedList<NameValuePair>();
		postFields.add(new BasicNameValuePair("uh", user.modhash));

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
					notifyFailure(RequestFailureType.PARSE, t, null, "JSON failed to parse");
				}

				responseHandler.notifySuccess(findCaptchaId(result));
			}

			@Override
			protected void onCallbackException(Throwable t) {
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onFailure(RequestFailureType type, Throwable t, StatusLine status, String readableMessage) {
				responseHandler.notifyFailure(type, t, status, readableMessage);
			}
		});
	}

	public static enum RedditAction {
		UPVOTE, UNVOTE, DOWNVOTE, SAVE, HIDE, UNSAVE, UNHIDE, REPORT
	}

	public static void action(final CacheManager cm,
							  final APIResponseHandler.ActionResponseHandler responseHandler,
							  final RedditAccount user,
							  final String idAndType,
							  final RedditAction action,
							  final Context context) {

		final LinkedList<NameValuePair> postFields = new LinkedList<NameValuePair>();
		postFields.add(new BasicNameValuePair("id", idAndType));
		postFields.add(new BasicNameValuePair("uh", user.modhash));

		final URI url = prepareActionUri(action, postFields);

		cm.makeRequest(new APIPostRequest(url, user, postFields, context) {
			@Override
			protected void onCallbackException(final Throwable t) {
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {
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
					notifyFailure(RequestFailureType.PARSE, t, null, "JSON failed to parse");
				}

				responseHandler.notifySuccess();
			}
		});
	}

	private static URI prepareActionUri(final RedditAction action, final LinkedList<NameValuePair> postFields) {
		switch(action) {
			case DOWNVOTE:
				postFields.add(new BasicNameValuePair("dir", "-1"));
				return Constants.Reddit.getUri(Constants.Reddit.PATH_VOTE);

			case UNVOTE:
				postFields.add(new BasicNameValuePair("dir", "0"));
				return Constants.Reddit.getUri(Constants.Reddit.PATH_VOTE);

			case UPVOTE:
				postFields.add(new BasicNameValuePair("dir", "1"));
				return Constants.Reddit.getUri(Constants.Reddit.PATH_VOTE);

			case SAVE: return Constants.Reddit.getUri(Constants.Reddit.PATH_SAVE);
			case HIDE: return Constants.Reddit.getUri(Constants.Reddit.PATH_HIDE);
			case UNSAVE: return Constants.Reddit.getUri(Constants.Reddit.PATH_UNSAVE);
			case UNHIDE: return Constants.Reddit.getUri(Constants.Reddit.PATH_UNHIDE);
			case REPORT: return Constants.Reddit.getUri(Constants.Reddit.PATH_REPORT);

			default:
				throw new RuntimeException("Unknown post action");
		}
	}

	// TODO just put this in the main menu fragment
	public static void getUserSubreddits(final CacheManager cm,
										 final APIResponseHandler.SubredditResponseHandler responseHandler,
										 final RedditAccount user,
										 final CacheRequest.DownloadType downloadType,
										 final boolean cancelExisting,
										 final Context context) {
		final URI uri;
		if(user.isAnonymous()) {
			uri = Constants.Reddit.getUri(Constants.Reddit.PATH_SUBREDDITS_POPULAR);
		} else {
			uri = Constants.Reddit.getUri(Constants.Reddit.PATH_SUBREDDITS_MINE_SUBSCRIBER);
		}

		cm.makeRequest(new APIGetRequest(uri, user, Constants.Priority.API_SUBREDDIT_LIST, Constants.FileType.SUBREDDIT_LIST, downloadType, true, cancelExisting, context) {

			@Override
			protected void onDownloadNecessary() {
				responseHandler.notifyDownloadNecessary();
			}

			@Override
			protected void onDownloadStarted() {
				responseHandler.notifyDownloadStarted();
			}

			@Override
			protected void onCallbackException(final Throwable t) {
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {
				responseHandler.notifyFailure(type, t, status, readableMessage);
			}

			@Override
			public void onJsonParseStarted(final JsonValue result, final long timestamp, final UUID session, final boolean fromCache) {

				final ArrayList<RedditSubreddit> output = new ArrayList<RedditSubreddit>();

				try {

					final JsonBufferedArray subreddits = result.asObject().getObject("data").getArray("children");

					for(final JsonValue v : subreddits) {
						final RedditThing thing = v.asObject(RedditThing.class);
						final RedditSubreddit subreddit = thing.asSubreddit();

						output.add(subreddit);
					}

					Collections.sort(output);

					responseHandler.notifySuccess(output, timestamp);

				} catch(Throwable t) {
					// TODO look for error
					notifyFailure(RequestFailureType.PARSE, t, null, "JSON parse failed for unknown reason");
				}
			}
		});
	}

	public static void getUser(final CacheManager cm,
							   final String usernameToGet,
							   final APIResponseHandler.UserResponseHandler responseHandler,
							   final RedditAccount user,
							   final CacheRequest.DownloadType downloadType,
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
			protected void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {
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
					notifyFailure(RequestFailureType.PARSE, t, null, "JSON parse failed for unknown reason");
				}
			}
		});
	}

	// lol, reddit api
	private static APIResponseHandler.APIFailureType findFailureType(final JsonValue response) {

		// TODO doesn't handle unknown failures

		// TODO handle 403 forbidden

		switch(response.getType()) {

			case OBJECT:

				for(final Map.Entry<String, JsonValue> v : response.asObject()) {
					final APIResponseHandler.APIFailureType failureType = findFailureType(v.getValue());
					if(failureType != null) return failureType;
				}

				break;

			case ARRAY:

				for(final JsonValue v : response.asArray()) {
					final APIResponseHandler.APIFailureType failureType = findFailureType(v);
					if(failureType != null) return failureType;
				}

				break;

			case STRING:

				if(Constants.Reddit.isApiErrorUser(response.asString()))
					return APIResponseHandler.APIFailureType.INVALID_USER;

				if(Constants.Reddit.isApiErrorCaptcha(response.asString()))
					return APIResponseHandler.APIFailureType.BAD_CAPTCHA;

				if(Constants.Reddit.isApiErrorNotAllowed(response.asString()))
					return APIResponseHandler.APIFailureType.NOTALLOWED;

				if(Constants.Reddit.isApiErrorSubredditRequired(response.asString()))
					return APIResponseHandler.APIFailureType.SUBREDDIT_REQUIRED;

				break;

			default:
				// Ignore
		}

		return null;
	}

	// lol, reddit api
	private static String findCaptchaId(final JsonValue response) {

		switch(response.getType()) {

			case OBJECT:

				for(final Map.Entry<String, JsonValue> v : response.asObject()) {
					final String captchaId = findCaptchaId(v.getValue());
					if(captchaId != null) return captchaId;
				}

				break;

			case ARRAY:

				for(final JsonValue v : response.asArray()) {
					final String captchaId = findCaptchaId(v);
					if(captchaId != null) return captchaId;
				}

				break;

			case STRING:

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

		public APIPostRequest(final URI url, final RedditAccount user, final List<NameValuePair> postFields, final Context context) {
			super(url, user, null, Constants.Priority.API_ACTION, 0, DownloadType.FORCE, Constants.FileType.NOCACHE, true, true, postFields, false, false, context);
		}

		@Override
		protected final void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {
			throw new RuntimeException("onSuccess called for uncached request");
		}

		@Override
		protected final void onProgress(final long bytesRead, final long totalBytes) {
			throw new RuntimeException("onProgress called for uncached request");
		}

		@Override
		public abstract void onJsonParseStarted(JsonValue result, long timestamp, UUID session, boolean fromCache);
	}

	// TODO merge get and post into one?
	private static abstract class APIGetRequest extends CacheRequest {

		public APIGetRequest(final URI url, final RedditAccount user, final int priority, final int fileType, final DownloadType downloadType, final boolean cache, final boolean cancelExisting, final Context context) {
			super(url, user, null, priority, 0, downloadType, fileType, true, true, null, cache, cancelExisting, context);
		}

		@Override
		protected final void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {
			if(!cache) throw new RuntimeException("onSuccess called for uncached request");
		}

		@Override
		protected final void onProgress(final long bytesRead, final long totalBytes) {
			if(!cache) throw new RuntimeException("onProgress called for uncached request");
		}

		@Override
		public abstract void onJsonParseStarted(JsonValue result, long timestamp, UUID session, boolean fromCache);
	}
}
