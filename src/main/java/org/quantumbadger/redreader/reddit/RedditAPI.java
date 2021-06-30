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
import androidx.annotation.IntDef;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.CacheRequestCallbacks;
import org.quantumbadger.redreader.cache.CacheRequestJSONParser;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategy;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyAlways;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.GenericFactory;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.TimestampBound;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;
import org.quantumbadger.redreader.http.HTTPBackend;
import org.quantumbadger.redreader.io.RequestResponseHandler;
import org.quantumbadger.redreader.jsonwrap.JsonArray;
import org.quantumbadger.redreader.jsonwrap.JsonString;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.reddit.api.SubredditRequestFailure;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.things.RedditThing;
import org.quantumbadger.redreader.reddit.things.RedditUser;
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId;

import java.io.IOException;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;


public final class RedditAPI {

	private static final String TAG = "RedditAPI";

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

	@IntDef({
			ACTION_UPVOTE,
			ACTION_UNVOTE,
			ACTION_DOWNVOTE,
			ACTION_SAVE,
			ACTION_HIDE,
			ACTION_UNSAVE,
			ACTION_UNHIDE,
			ACTION_REPORT,
			ACTION_DELETE})
	@Retention(RetentionPolicy.SOURCE)
	public @interface RedditAction {
	}

	@IntDef({SUBSCRIPTION_ACTION_SUBSCRIBE, SUBSCRIPTION_ACTION_UNSUBSCRIBE})
	@Retention(RetentionPolicy.SOURCE)
	public @interface RedditSubredditAction {
	}

	private static final class GenericResponseHandler implements CacheRequestJSONParser.Listener {

		@NonNull private final APIResponseHandler.ActionResponseHandler mHandler;

		private GenericResponseHandler(
				@NonNull final APIResponseHandler.ActionResponseHandler handler) {
			mHandler = handler;
		}

		@Override
		public void onJsonParsed(
				@NonNull final JsonValue result,
				final long timestamp,
				@NonNull final UUID session,
				final boolean fromCache) {

			try {
				final APIResponseHandler.APIFailureType failureType = findFailureType(result);

				if(failureType != null) {
					mHandler.notifyFailure(failureType, "GenericResponseHandler", result);
				} else {
					mHandler.notifySuccess();
				}

			} catch(final Exception e) {
				BugReportActivity.handleGlobalError(mHandler.context, new RRError(
						null,
						null,
						true,
						e,
						null,
						null,
						result.toString()));
			}
		}

		@Override
		public void onFailure(
				final int type,
				@Nullable final Throwable t,
				@Nullable final Integer httpStatus,
				@Nullable final String readableMessage) {

			mHandler.notifyFailure(type, t, httpStatus, readableMessage, null);
		}
	}

	public interface FlairSelectorResponseHandler {

		void onSuccess(@NonNull Collection<RedditFlairChoice> choices);

		void onSubredditDoesNotExist();

		void onSubredditPermissionDenied();

		void onFailure(
				int type,
				@Nullable Throwable t,
				@Nullable Integer httpStatus,
				@Nullable String readableMessage,
				@Nullable JsonValue response);

		void onFailure(
				@NonNull APIResponseHandler.APIFailureType type,
				@Nullable JsonValue response);
	}

	public static void flairSelectorForNewLink(
			@NonNull final Context context,
			@NonNull final CacheManager cm,
			@NonNull final RedditAccount user,
			@NonNull final SubredditCanonicalId subreddit,
			@NonNull final FlairSelectorResponseHandler responseHandler) {

		final LinkedList<HTTPBackend.PostField> postFields = new LinkedList<>();
		postFields.add(new HTTPBackend.PostField("is_newlink", "true"));

		cm.makeRequest(createPostRequest(
				Constants.Reddit.getUri(subreddit + "/api/flairselector"),
				user,
				postFields,
				context,
				new CacheRequestJSONParser.Listener() {

					@Override
					public void onJsonParsed(
							@NonNull final JsonValue result,
							final long timestamp,
							@NonNull final UUID session,
							final boolean fromCache) {

						if(result.asObject() != null
								&& Objects.requireNonNull(result.asObject()).isEmpty()) {
							responseHandler.onSuccess(Collections.emptyList());
							return;
						}

						if(result.asString() != null
								&& Objects.requireNonNull(result.asString()).equals("{}")) {
							responseHandler.onSuccess(Collections.emptyList());
							return;
						}

						final Optional<JsonArray> array = result.getArrayAtPath("choices");

						if(array.isEmpty()) {

							final APIResponseHandler.APIFailureType failureType
									= findFailureType(result);

							if(failureType != null) {
								responseHandler.onFailure(failureType, result);
							} else {
								responseHandler.onFailure(
										APIResponseHandler.APIFailureType.UNKNOWN,
										result);
							}

							return;
						}

						final Optional<List<RedditFlairChoice>> choices
								= RedditFlairChoice.fromJsonList(array.get());

						if(choices.isEmpty()) {
							responseHandler.onFailure(
									CacheRequest.REQUEST_FAILURE_PARSE,
									new RuntimeException(),
									null,
									"Failed to parse choices list",
									result);
							return;
						}

						responseHandler.onSuccess(choices.get());
					}

					@Override
					public void onFailure(
							final int type,
							@Nullable final Throwable t,
							@Nullable final Integer httpStatus,
							@Nullable final String readableMessage) {

						if(httpStatus != null && httpStatus == 404) {
							responseHandler.onSubredditDoesNotExist();

						} else if(httpStatus != null && httpStatus == 403) {
							responseHandler.onSubredditPermissionDenied();

						} else {
							responseHandler.onFailure(type, t, httpStatus, readableMessage, null);
						}
					}
				}));
	}

	private static class SubmitJSONListener implements CacheRequestJSONParser.Listener {

		@NonNull private final APIResponseHandler.SubmitResponseHandler mResponseHandler;

		private SubmitJSONListener(
				@NonNull final APIResponseHandler.SubmitResponseHandler responseHandler) {
			mResponseHandler = responseHandler;
		}

		@Override
		public void onJsonParsed(
				@NonNull final JsonValue result,
				final long timestamp,
				@NonNull final UUID session,
				final boolean fromCache) {

			try {
				final Optional<JsonArray> errorsJson
						= result.getArrayAtPath("json", "errors");

				if(errorsJson.isPresent()) {

					final ArrayList<String> errors = new ArrayList<>();

					for(final JsonValue errorValue : errorsJson.get()) {

						final JsonArray error = errorValue.asArray();

						if(error != null && error.getString(1) != null) {
							errors.add(error.getString(1));
						}
					}

					if(!errors.isEmpty()) {
						mResponseHandler.onSubmitErrors(errors);
						return;
					}
				}

				final APIResponseHandler.APIFailureType failureType
						= findFailureType(result);

				if(failureType != null) {
					mResponseHandler.notifyFailure(
							failureType,
							null,
							result);
				} else {
					mResponseHandler.onSuccess(
							result.getStringAtPath("json", "data", "things", 0, "data", "permalink")
									.orElse(result.getStringAtPath("json", "data", "url")),
							result.getStringAtPath("json", "data", "things", 0, "data", "name"));
				}

			} catch(final Exception e) {
				BugReportActivity.handleGlobalError(
						mResponseHandler.context,
						new RRError(
								null,
								null,
								true,
								e,
								null,
								null,
								result.toString()));
			}
		}

		@Override
		public void onFailure(
				final int type,
				@Nullable final Throwable t,
				@Nullable final Integer httpStatus,
				@Nullable final String readableMessage) {

			mResponseHandler.notifyFailure(type, t, httpStatus, readableMessage, null);
		}
	}

	public static void submit(
			final CacheManager cm,
			final APIResponseHandler.SubmitResponseHandler responseHandler,
			final RedditAccount user,
			final boolean is_self,
			final String subreddit,
			final String title,
			final String body,
			final boolean sendRepliesToInbox,
			final boolean markAsNsfw,
			final boolean markAsSpoiler,
			@Nullable final String flairId,
			final Context context) {

		final LinkedList<HTTPBackend.PostField> postFields = new LinkedList<>();
		postFields.add(new HTTPBackend.PostField("api_type", "json"));
		postFields.add(new HTTPBackend.PostField("kind", is_self ? "self" : "link"));
		postFields.add(new HTTPBackend.PostField(
				"sendreplies",
				sendRepliesToInbox ? "true" : "false"));
		postFields.add(new HTTPBackend.PostField("nsfw", markAsNsfw ? "true" : "false"));
		postFields.add(new HTTPBackend.PostField("spoiler", markAsSpoiler ? "true" : "false"));
		postFields.add(new HTTPBackend.PostField("sr", subreddit));
		postFields.add(new HTTPBackend.PostField("title", title));

		if(flairId != null) {
			postFields.add(new HTTPBackend.PostField("flair_id", flairId));
		}

		if(is_self) {
			postFields.add(new HTTPBackend.PostField("text", body));
		} else {
			postFields.add(new HTTPBackend.PostField("url", body));
		}

		cm.makeRequest(createPostRequest(
				Constants.Reddit.getUri("/api/submit"),
				user,
				postFields,
				context,
				new SubmitJSONListener(responseHandler)));
	}

	public static void compose(
			@NonNull final CacheManager cm,
			@NonNull final APIResponseHandler.ActionResponseHandler responseHandler,
			@NonNull final RedditAccount user,
			@NonNull final String recipient,
			@NonNull final String subject,
			@NonNull final String body,
			@NonNull final Context context) {

		final LinkedList<HTTPBackend.PostField> postFields = new LinkedList<>();
		postFields.add(new HTTPBackend.PostField("api_type", "json"));
		postFields.add(new HTTPBackend.PostField("subject", subject));
		postFields.add(new HTTPBackend.PostField("to", recipient));
		postFields.add(new HTTPBackend.PostField("text", body));

		cm.makeRequest(createPostRequest(
				Constants.Reddit.getUri("/api/compose"),
				user,
				postFields,
				context,
				new GenericResponseHandler(responseHandler)));
	}

	public static void comment(
			final CacheManager cm,
			final APIResponseHandler.SubmitResponseHandler responseHandler,
			final APIResponseHandler.ActionResponseHandler inboxResponseHandler,
			final RedditAccount user,
			final String parentIdAndType,
			final String markdown,
			final boolean sendRepliesToInbox,
			final AppCompatActivity context) {

		final LinkedList<HTTPBackend.PostField> postFields = new LinkedList<>();
		postFields.add(new HTTPBackend.PostField("api_type", "json"));
		postFields.add(new HTTPBackend.PostField("thing_id", parentIdAndType));
		postFields.add(new HTTPBackend.PostField("text", markdown));

		cm.makeRequest(createPostRequest(
				Constants.Reddit.getUri("/api/comment"),
				user,
				postFields,
				context,
				new SubmitJSONListener(new APIResponseHandler.SubmitResponseHandler(context) {
					@Override
					public void onSubmitErrors(@NonNull final ArrayList<String> errors) {
						responseHandler.onSubmitErrors(errors);
					}

					@Override
					public void onSuccess(
							@NonNull final Optional<String> redirectUrl,
							@NonNull final Optional<String> thingId) {

						if(!sendRepliesToInbox) {
							thingId.ifPresent(commentFullname -> sendReplies(
									cm,
									inboxResponseHandler,
									user,
									commentFullname,
									false,
									context));
						}

						responseHandler.onSuccess(redirectUrl, thingId);
					}

					@Override
					protected void onCallbackException(final Throwable t) {
						responseHandler.onCallbackException(t);
					}

					@Override
					protected void onFailure(
							final int type,
							@Nullable final Throwable t,
							@Nullable final Integer status,
							@Nullable final String readableMessage,
							@Nullable final JsonValue response) {

						responseHandler.onFailure(type, t, status, readableMessage, response);
					}

					@Override
					protected void onFailure(
							@NonNull final APIFailureType type,
							@Nullable final String debuggingContext,
							@Nullable final JsonValue response) {

						responseHandler.onFailure(type, debuggingContext, response);
					}
				})));
	}

	public static void markAllAsRead(
			final CacheManager cm,
			final APIResponseHandler.ActionResponseHandler responseHandler,
			final RedditAccount user,
			final Context context) {

		final LinkedList<HTTPBackend.PostField> postFields = new LinkedList<>();

		cm.makeRequest(createPostRequestUnprocessedResponse(
				Constants.Reddit.getUri("/api/read_all_messages"),
				user,
				postFields,
				context,
				new CacheRequestCallbacks() {
					@Override
					public void onFailure(
							final int type,
							@Nullable final Throwable t,
							@Nullable final Integer httpStatus,
							@Nullable final String readableMessage) {

						responseHandler.notifyFailure(type, t, httpStatus, readableMessage, null);
					}

					@Override
					public void onDataStreamComplete(
							@NonNull final GenericFactory<SeekableInputStream, IOException> stream,
							final long timestamp,
							@NonNull final UUID session,
							final boolean fromCache,
							@Nullable final String mimetype) {

						responseHandler.notifySuccess();
					}
				}));
	}

	public static void editComment(
			final CacheManager cm,
			final APIResponseHandler.ActionResponseHandler responseHandler,
			final RedditAccount user,
			final String commentIdAndType,
			final String markdown,
			final Context context) {

		final LinkedList<HTTPBackend.PostField> postFields = new LinkedList<>();
		postFields.add(new HTTPBackend.PostField("thing_id", commentIdAndType));
		postFields.add(new HTTPBackend.PostField("text", markdown));

		cm.makeRequest(createPostRequest(
				Constants.Reddit.getUri("/api/editusertext"),
				user,
				postFields,
				context,
				new GenericResponseHandler(responseHandler)));
	}

	public static void action(
			final CacheManager cm,
			final APIResponseHandler.ActionResponseHandler responseHandler,
			final RedditAccount user,
			final String idAndType,
			final @RedditAction int action,
			final Context context) {

		final LinkedList<HTTPBackend.PostField> postFields = new LinkedList<>();
		postFields.add(new HTTPBackend.PostField("id", idAndType));

		final URI url = prepareActionUri(action, postFields);

		cm.makeRequest(createPostRequest(
				url,
				user,
				postFields,
				context,
				new GenericResponseHandler(responseHandler)));
	}

	private static URI prepareActionUri(
			final @RedditAction int action,
			final LinkedList<HTTPBackend.PostField> postFields) {
		switch(action) {
			case ACTION_DOWNVOTE:
				postFields.add(new HTTPBackend.PostField("dir", "-1"));
				return Constants.Reddit.getUri(Constants.Reddit.PATH_VOTE);

			case ACTION_UNVOTE:
				postFields.add(new HTTPBackend.PostField("dir", "0"));
				return Constants.Reddit.getUri(Constants.Reddit.PATH_VOTE);

			case ACTION_UPVOTE:
				postFields.add(new HTTPBackend.PostField("dir", "1"));
				return Constants.Reddit.getUri(Constants.Reddit.PATH_VOTE);

			case ACTION_SAVE:
				return Constants.Reddit.getUri(Constants.Reddit.PATH_SAVE);
			case ACTION_HIDE:
				return Constants.Reddit.getUri(Constants.Reddit.PATH_HIDE);
			case ACTION_UNSAVE:
				return Constants.Reddit.getUri(Constants.Reddit.PATH_UNSAVE);
			case ACTION_UNHIDE:
				return Constants.Reddit.getUri(Constants.Reddit.PATH_UNHIDE);
			case ACTION_REPORT:
				return Constants.Reddit.getUri(Constants.Reddit.PATH_REPORT);
			case ACTION_DELETE:
				return Constants.Reddit.getUri(Constants.Reddit.PATH_DELETE);

			default:
				throw new RuntimeException("Unknown post/comment action");
		}
	}

	public static void subscriptionAction(
			final CacheManager cm,
			final APIResponseHandler.ActionResponseHandler responseHandler,
			final RedditAccount user,
			final SubredditCanonicalId subredditId,
			final @RedditSubredditAction int action,
			final Context context) {

		RedditSubredditManager.getInstance(context, user).getSubreddit(
				subredditId,
				TimestampBound.ANY,
				new RequestResponseHandler<RedditSubreddit, SubredditRequestFailure>() {

					@Override
					public void onRequestFailed(final SubredditRequestFailure failureReason) {
						responseHandler.notifyFailure(
								failureReason.requestFailureType,
								failureReason.t,
								failureReason.statusLine,
								failureReason.readableMessage,
								null);
					}

					@Override
					public void onRequestSuccess(
							final RedditSubreddit subreddit,
							final long timeCached) {

						final LinkedList<HTTPBackend.PostField> postFields = new LinkedList<>();

						postFields.add(new HTTPBackend.PostField("sr", subreddit.name));

						final URI url = subscriptionPrepareActionUri(action, postFields);

						cm.makeRequest(createPostRequest(
								url,
								user,
								postFields,
								context,
								new GenericResponseHandler(responseHandler)));
					}
				},
				null);
	}

	private static URI subscriptionPrepareActionUri(
			final @RedditSubredditAction int action,
			final LinkedList<HTTPBackend.PostField> postFields) {
		switch(action) {
			case SUBSCRIPTION_ACTION_SUBSCRIBE:
				postFields.add(new HTTPBackend.PostField("action", "sub"));
				return Constants.Reddit.getUri(Constants.Reddit.PATH_SUBSCRIBE);

			case SUBSCRIPTION_ACTION_UNSUBSCRIBE:
				postFields.add(new HTTPBackend.PostField("action", "unsub"));
				return Constants.Reddit.getUri(Constants.Reddit.PATH_SUBSCRIBE);

			default:
				throw new RuntimeException("Unknown subreddit action");
		}
	}

	public static void getUser(
			final CacheManager cm,
			final String usernameToGet,
			final APIResponseHandler.UserResponseHandler responseHandler,
			final RedditAccount user,
			final DownloadStrategy downloadStrategy,
			final Context context) {

		final URI uri = Constants.Reddit.getUri("/user/" + usernameToGet + "/about.json");

		cm.makeRequest(createGetRequest(
				uri,
				user,
				new Priority(Constants.Priority.API_USER_ABOUT),
				Constants.FileType.USER_ABOUT,
				downloadStrategy,
				context,
				new CacheRequestJSONParser.Listener() {
					@Override
					public void onJsonParsed(
							@NonNull final JsonValue result,
							final long timestamp,
							@NonNull final UUID session,
							final boolean fromCache) {

						try {
							final RedditThing userThing = result.asObject(RedditThing.class);
							final RedditUser userResult = userThing.asUser();
							responseHandler.notifySuccess(userResult, timestamp);

						} catch(final Throwable t) {
							// TODO look for error
							responseHandler.notifyFailure(
									CacheRequest.REQUEST_FAILURE_PARSE,
									t,
									null,
									"JSON parse failed for unknown reason",
									result);
						}
					}

					@Override
					public void onFailure(
							final int type,
							@Nullable final Throwable t,
							@Nullable final Integer httpStatus,
							@Nullable final String readableMessage) {
						responseHandler.notifyFailure(type, t, httpStatus, readableMessage, null);
					}
				}));
	}

	public static void sendReplies(
			final CacheManager cm,
			final APIResponseHandler.ActionResponseHandler responseHandler,
			final RedditAccount user,
			final String fullname,
			final boolean state,
			final Context context) {

		final LinkedList<HTTPBackend.PostField> postFields = new LinkedList<>();
		postFields.add(new HTTPBackend.PostField("id", fullname));
		postFields.add(new HTTPBackend.PostField("state", String.valueOf(state)));
		cm.makeRequest(createPostRequest(
				Constants.Reddit.getUri("/api/sendreplies"),
				user,
				postFields,
				context,
				new GenericResponseHandler(responseHandler)));
	}

	// lol, reddit api
	@Nullable
	private static APIResponseHandler.APIFailureType findFailureType(final JsonValue response) {

		// TODO handle 403 forbidden

		if(response == null) {
			return null;
		}

		boolean unknownError = false;

		if(response.asObject() != null) {

			for(final Map.Entry<String, JsonValue> v : response.asObject()) {

				if("success".equals(v.getKey())
						&& Boolean.FALSE.equals(v.getValue().asBoolean())) {

					unknownError = true;
				}

				final APIResponseHandler.APIFailureType failureType =
						findFailureType(v.getValue());

				if(failureType == APIResponseHandler.APIFailureType.UNKNOWN) {
					unknownError = true;

				} else if(failureType != null) {
					return failureType;
				}
			}

			final Optional<JsonArray> errors = response.getArrayAtPath("json", "errors");

			if(errors.isPresent() && errors.get().size() > 0) {
				unknownError = true;
			}

		} else if(response.asArray() != null) {

			for(final JsonValue v : response.asArray()) {
				final APIResponseHandler.APIFailureType failureType =
						findFailureType(v);

				if(failureType == APIResponseHandler.APIFailureType.UNKNOWN) {
					unknownError = true;

				} else if(failureType != null) {
					return failureType;
				}
			}

		} else if(response instanceof JsonString) {

			final String responseAsString = response.asString();

			if(Constants.Reddit.isApiErrorUser(responseAsString)) {
				return APIResponseHandler.APIFailureType.INVALID_USER;
			}

			if(Constants.Reddit.isApiErrorCaptcha(responseAsString)) {
				return APIResponseHandler.APIFailureType.BAD_CAPTCHA;
			}

			if(Constants.Reddit.isApiErrorNotAllowed(responseAsString)) {
				return APIResponseHandler.APIFailureType.NOTALLOWED;
			}

			if(Constants.Reddit.isApiErrorSubredditRequired(responseAsString)) {
				return APIResponseHandler.APIFailureType.SUBREDDIT_REQUIRED;
			}

			if(Constants.Reddit.isApiErrorURLRequired(responseAsString)) {
				return APIResponseHandler.APIFailureType.URL_REQUIRED;
			}

			if(Constants.Reddit.isApiTooFast(responseAsString)) {
				return APIResponseHandler.APIFailureType.TOO_FAST;
			}

			if(Constants.Reddit.isApiTooLong(responseAsString)) {
				return APIResponseHandler.APIFailureType.TOO_LONG;
			}

			if(Constants.Reddit.isApiAlreadySubmitted(responseAsString)) {
				return APIResponseHandler.APIFailureType.ALREADY_SUBMITTED;
			}

			if(Constants.Reddit.isPostFlairRequired(responseAsString)) {
				return APIResponseHandler.APIFailureType.POST_FLAIR_REQUIRED;
			}

			if(Constants.Reddit.isApiError(responseAsString)) {
				unknownError = true;
			}
		}

		return unknownError ? APIResponseHandler.APIFailureType.UNKNOWN : null;
	}

	@NonNull
	private static CacheRequest createPostRequest(
			@NonNull final URI url,
			@NonNull final RedditAccount user,
			@NonNull final List<HTTPBackend.PostField> postFields,
			@NonNull final Context context,
			@NonNull final CacheRequestJSONParser.Listener handler) {

		return createPostRequestUnprocessedResponse(
				url,
				user,
				postFields,
				context,
				new CacheRequestJSONParser(context, handler));
	}

	@NonNull
	private static CacheRequest createPostRequestUnprocessedResponse(
			@NonNull final URI url,
			@NonNull final RedditAccount user,
			@NonNull final List<HTTPBackend.PostField> postFields,
			@NonNull final Context context,
			@NonNull final CacheRequestCallbacks callbacks) {

		return new CacheRequest(
				url,
				user,
				null,
				new Priority(Constants.Priority.API_ACTION),
				DownloadStrategyAlways.INSTANCE,
				Constants.FileType.NOCACHE,
				CacheRequest.DOWNLOAD_QUEUE_REDDIT_API,
				postFields,
				context,
				callbacks);
	}

	@NonNull
	private static CacheRequest createGetRequest(
			@NonNull final URI url,
			@NonNull final RedditAccount user,
			@NonNull final Priority priority,
			final int fileType,
			@NonNull final DownloadStrategy downloadStrategy,
			@NonNull final Context context,
			@NonNull final CacheRequestJSONParser.Listener handler) {

		return new CacheRequest(
				url,
				user,
				null,
				priority,
				downloadStrategy,
				fileType,
				CacheRequest.DOWNLOAD_QUEUE_REDDIT_API,
				null,
				context,
				new CacheRequestJSONParser(context, handler));
	}
}
