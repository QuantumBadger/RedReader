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

package org.quantumbadger.redreader.reddit.api;

import android.app.AlertDialog;
import android.app.ProgressDialog;
import android.content.Context;
import android.net.Uri;
import android.os.Build;
import android.os.SystemClock;
import android.util.Base64;
import android.view.KeyEvent;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.RunnableOnce;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.http.HTTPBackend;
import org.quantumbadger.redreader.http.PostField;
import org.quantumbadger.redreader.http.body.HTTPRequestBodyPostFields;
import org.quantumbadger.redreader.jsonwrap.JsonObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

public final class RedditOAuth {

	private static final String CLIENT_ID_OLD = "m_zCW1Dixs9WLA";

	private static final String REDIRECT_URI_NEW = "redreader://rr_oauth_redir";
	private static final String CLIENT_ID_NEW = "EvLqme1n5YC93w";

	private static final String ALL_SCOPES = "identity edit flair history "
			+ "modconfig modflair modlog modposts modwiki mysubreddits "
			+ "privatemessages read report save submit subscribe vote "
			+ "wikiedit wikiread";

	private static final String ACCESS_TOKEN_URL =
			"https://www.reddit.com/api/v1/access_token";

	public static class Token {

		public final String token;

		public Token(final String token) {
			this.token = token;
		}

		@Override
		public String toString() {
			return token;
		}
	}

	public static final class AccessToken extends Token {

		private final long mMonotonicTimestamp;

		public AccessToken(final String token) {
			super(token);
			mMonotonicTimestamp = SystemClock.elapsedRealtime();
		}

		public boolean isExpired() {
			final long halfHourInMs = 30 * 60 * 1000;
			return mMonotonicTimestamp + halfHourInMs < SystemClock.elapsedRealtime();
		}
	}

	public static final class RefreshToken extends Token {
		public RefreshToken(final String token) {
			super(token);
		}
	}

	private enum FetchRefreshTokenResultStatus {
		SUCCESS,
		USER_REFUSED_PERMISSION,
		INVALID_REQUEST,
		INVALID_RESPONSE,
		CONNECTION_ERROR,
		UNKNOWN_ERROR
	}

	private enum FetchUserInfoResultStatus {
		SUCCESS,
		INVALID_RESPONSE,
		CONNECTION_ERROR,
		UNKNOWN_ERROR
	}

	private static final class FetchRefreshTokenResult {

		public final FetchRefreshTokenResultStatus status;
		public final RRError error;

		public final RefreshToken refreshToken;
		public final AccessToken accessToken;

		public FetchRefreshTokenResult(
				final FetchRefreshTokenResultStatus status,
				final RRError error) {
			this.status = status;
			this.error = error;
			this.refreshToken = null;
			this.accessToken = null;
		}

		public FetchRefreshTokenResult(
				final RefreshToken refreshToken,
				final AccessToken accessToken) {
			this.status = FetchRefreshTokenResultStatus.SUCCESS;
			this.error = null;
			this.refreshToken = refreshToken;
			this.accessToken = accessToken;
		}
	}

	private static final class FetchUserInfoResult {

		public final FetchUserInfoResultStatus status;
		public final RRError error;

		public final String username;

		public FetchUserInfoResult(
				final FetchUserInfoResultStatus status,
				final RRError error) {
			this.status = status;
			this.error = error;
			this.username = null;
		}

		public FetchUserInfoResult(final String username) {
			this.status = FetchUserInfoResultStatus.SUCCESS;
			this.error = null;
			this.username = username;
		}
	}

	public static Uri getPromptUri() {

		final Uri.Builder uri =
				Uri.parse("https://www.reddit.com/api/v1/authorize.compact").buildUpon();

		uri.appendQueryParameter("response_type", "code");
		uri.appendQueryParameter("duration", "permanent");
		uri.appendQueryParameter("state", "Texas");
		uri.appendQueryParameter("redirect_uri", REDIRECT_URI_NEW);
		uri.appendQueryParameter("client_id", CLIENT_ID_NEW);
		uri.appendQueryParameter("scope", ALL_SCOPES);

		return uri.build();
	}

	private static FetchRefreshTokenResult handleRefreshTokenError(
			final Throwable exception,
			final Integer httpStatus,
			final Context context,
			final String uri) {

		if(httpStatus != null && httpStatus != 200) {
			return new FetchRefreshTokenResult(
					FetchRefreshTokenResultStatus.UNKNOWN_ERROR,
					new RRError(
							context.getString(R.string.error_unknown_title),
							context.getString(R.string.message_cannotlogin),
							true,
							null,
							httpStatus,
							uri,
							null)
			);

		} else if(exception instanceof IOException) {
			return new FetchRefreshTokenResult(
					FetchRefreshTokenResultStatus.CONNECTION_ERROR,
					new RRError(
							context.getString(R.string.error_connection_title),
							context.getString(R.string.error_connection_message),
							true,
							exception,
							null,
							uri,
							null)
			);

		} else {
			return new FetchRefreshTokenResult(
					FetchRefreshTokenResultStatus.UNKNOWN_ERROR,
					new RRError(
							context.getString(R.string.error_unknown_title),
							context.getString(R.string.error_unknown_message),
							true,
							exception,
							null,
							uri,
							null)
			);
		}
	}

	private static FetchAccessTokenResult handleAccessTokenError(
			final Throwable exception,
			final Integer httpStatus,
			final Context context,
			final String uri) {

		if(httpStatus != null && httpStatus != 200) {
			return new FetchAccessTokenResult(
					FetchAccessTokenResultStatus.UNKNOWN_ERROR,
					new RRError(
							context.getString(R.string.error_unknown_title),
							context.getString(R.string.message_cannotlogin),
							true,
							null,
							httpStatus,
							uri,
							null)
			);

		} else if(exception instanceof IOException) {
			return new FetchAccessTokenResult(
					FetchAccessTokenResultStatus.CONNECTION_ERROR,
					new RRError(
							context.getString(R.string.error_connection_title),
							context.getString(R.string.error_connection_message),
							true,
							exception,
							null,
							uri,
							null)
			);

		} else {
			return new FetchAccessTokenResult(
					FetchAccessTokenResultStatus.UNKNOWN_ERROR,
					new RRError(
							context.getString(R.string.error_unknown_title),
							context.getString(R.string.error_unknown_message),
							true,
							exception,
							null,
							uri,
							null)
			);
		}
	}

	private static FetchRefreshTokenResult fetchRefreshTokenSynchronous(
			final Context context,
			final Uri redirectUri) {

		final String error = redirectUri.getQueryParameter("error");

		if(error != null) {

			if(error.equals("access_denied")) {
				return new FetchRefreshTokenResult(
						FetchRefreshTokenResultStatus.USER_REFUSED_PERMISSION,
						new RRError(
								context.getString(
										R.string.error_title_login_user_denied_permission),
								context.getString(
										R.string.error_message_login_user_denied_permission),
								false)
				);

			} else {
				return new FetchRefreshTokenResult(
						FetchRefreshTokenResultStatus.INVALID_REQUEST,
						new RRError(
								context.getString(
										R.string.error_title_login_unknown_reddit_error,
										error),
								context.getString(R.string.error_unknown_message),
								true));
			}
		}

		final String code = redirectUri.getQueryParameter("code");

		if(code == null) {
			return new FetchRefreshTokenResult(
					FetchRefreshTokenResultStatus.INVALID_RESPONSE,
					new RRError(
							context.getString(R.string.error_unknown_title),
							context.getString(R.string.error_unknown_message),
							true)
			);
		}

		final String uri = ACCESS_TOKEN_URL;

		final ArrayList<PostField> postFields = new ArrayList<>(3);
		postFields.add(new PostField("grant_type", "authorization_code"));
		postFields.add(new PostField("code", code));
		postFields.add(new PostField("redirect_uri", REDIRECT_URI_NEW));

		try {
			final HTTPBackend.Request request = HTTPBackend.getBackend().prepareRequest(
					context,
					new HTTPBackend.RequestDetails(
							General.uriFromString(uri),
							Optional.of(new HTTPRequestBodyPostFields(postFields))));

			request.addHeader(
					"Authorization",
					"Basic " + Base64.encodeToString(
							(CLIENT_ID_NEW + ":").getBytes(),
							Base64.URL_SAFE | Base64.NO_WRAP));

			final AtomicReference<FetchRefreshTokenResult> result =
					new AtomicReference<>();

			request.executeInThisThread(new HTTPBackend.Listener() {

				@Override
				public void onError(
						final @CacheRequest.RequestFailureType int failureType,
						final Throwable exception,
						final Integer httpStatus,
						@NonNull final Optional<FailedRequestBody> body) {
					result.set(handleRefreshTokenError(
							exception,
							httpStatus,
							context,
							uri));
				}

				@Override
				public void onSuccess(
						final String mimetype,
						final Long bodyBytes,
						final InputStream body) {

					try {
						final JsonValue jsonValue = JsonValue.parse(body);
						final JsonObject responseObject = jsonValue.asObject();

						final RefreshToken refreshToken =
								new RefreshToken(responseObject.getString("refresh_token"));
						final AccessToken accessToken =
								new AccessToken(responseObject.getString("access_token"));

						result.set(new FetchRefreshTokenResult(
								refreshToken,
								accessToken));

					} catch(final IOException e) {

						result.set(new FetchRefreshTokenResult(
								FetchRefreshTokenResultStatus.CONNECTION_ERROR,
								new RRError(
										context.getString(R.string.error_connection_title),
										context.getString(R.string.error_connection_message),
										true,
										e,
										null,
										uri,
										null)
						));

					} catch(final Throwable t) {
						throw new RuntimeException(t);

					} finally {
						General.closeSafely(body);
					}
				}
			});

			return result.get();

		} catch(final Throwable t) {
			return new FetchRefreshTokenResult(
					FetchRefreshTokenResultStatus.UNKNOWN_ERROR,
					new RRError(
							context.getString(R.string.error_unknown_title),
							context.getString(R.string.error_unknown_message),
							true,
							t,
							null,
							uri,
							null)
			);
		}
	}

	private static FetchUserInfoResult fetchUserInfoSynchronous(
			final Context context,
			final AccessToken accessToken) {

		final URI uri = Constants.Reddit.getUri(Constants.Reddit.PATH_ME);

		try {
			final HTTPBackend.Request request
					= HTTPBackend.getBackend()
					.prepareRequest(context, new HTTPBackend.RequestDetails(uri, Optional.empty()));

			request.addHeader("Authorization", "bearer " + accessToken.token);

			final AtomicReference<FetchUserInfoResult> result = new AtomicReference<>();

			request.executeInThisThread(new HTTPBackend.Listener() {

				@Override
				public void onError(
						final @CacheRequest.RequestFailureType int failureType,
						final Throwable exception,
						final Integer httpStatus,
						@NonNull final Optional<FailedRequestBody> body) {

					if(httpStatus != null && httpStatus != 200) {
						result.set(new FetchUserInfoResult(
								FetchUserInfoResultStatus.CONNECTION_ERROR,
								new RRError(
										context.getString(R.string.error_unknown_title),
										context.getString(R.string.error_unknown_message),
										true,
										null,
										httpStatus,
										uri.toString(),
										null,
										body)));

					} else {
						result.set(new FetchUserInfoResult(
								FetchUserInfoResultStatus.UNKNOWN_ERROR,
								new RRError(
										context.getString(R.string.error_unknown_title),
										context.getString(R.string.error_unknown_message),
										true,
										exception,
										null,
										uri.toString(),
										null,
										body)));
					}
				}

				@Override
				public void onSuccess(
						final String mimetype,
						final Long bodyBytes,
						final InputStream body) {

					try {

						final JsonValue jsonValue = JsonValue.parse(body);
						final JsonObject responseObject = jsonValue.asObject();

						final String username = responseObject.getString("name");

						if(username == null || username.isEmpty()) {

							result.set(new FetchUserInfoResult(
									FetchUserInfoResultStatus.INVALID_RESPONSE,
									new RRError(
											context.getString(R.string.error_unknown_title),
											context.getString(R.string.error_unknown_message),
											true,
											null,
											null,
											uri.toString(),
											null,
											Optional.of(new FailedRequestBody(jsonValue)))));

							return;
						}

						result.set(new FetchUserInfoResult(username));

					} catch(final IOException e) {
						result.set(new FetchUserInfoResult(
								FetchUserInfoResultStatus.CONNECTION_ERROR,
								new RRError(
										context.getString(R.string.error_connection_title),
										context.getString(R.string.error_connection_message),
										true,
										e,
										null,
										uri.toString(),
										null)
						));

					} catch(final Throwable t) {
						throw new RuntimeException(t);

					} finally {
						General.closeSafely(body);
					}
				}
			});

			return result.get();

		} catch(final Throwable t) {
			return new FetchUserInfoResult(
					FetchUserInfoResultStatus.UNKNOWN_ERROR,
					new RRError(
							context.getString(R.string.error_unknown_title),
							context.getString(R.string.error_unknown_message),
							true,
							t,
							null,
							uri.toString(),
							null)
			);
		}
	}

	public enum LoginError {
		SUCCESS,
		USER_REFUSED_PERMISSION,
		CONNECTION_ERROR,
		UNKNOWN_ERROR;

		static LoginError fromFetchRefreshTokenStatus(final FetchRefreshTokenResultStatus status) {
			switch(status) {
				case SUCCESS:
					return SUCCESS;
				case USER_REFUSED_PERMISSION:
					return USER_REFUSED_PERMISSION;
				case INVALID_REQUEST:
					return UNKNOWN_ERROR;
				case INVALID_RESPONSE:
					return UNKNOWN_ERROR;
				case CONNECTION_ERROR:
					return CONNECTION_ERROR;
				case UNKNOWN_ERROR:
					return UNKNOWN_ERROR;
			}

			return UNKNOWN_ERROR;
		}

		static LoginError fromFetchUserInfoStatus(final FetchUserInfoResultStatus status) {
			switch(status) {
				case SUCCESS:
					return SUCCESS;
				case INVALID_RESPONSE:
					return UNKNOWN_ERROR;
				case CONNECTION_ERROR:
					return CONNECTION_ERROR;
				case UNKNOWN_ERROR:
					return UNKNOWN_ERROR;
			}

			return UNKNOWN_ERROR;
		}
	}

	public interface LoginListener {
		void onLoginSuccess(RedditAccount account);

		void onLoginFailure(LoginError error, RRError details);
	}

	public static void loginAsynchronous(
			final Context context,
			final Uri redirectUri,
			final LoginListener listener) {

		new Thread() {
			@Override
			public void run() {
				try {

					final FetchRefreshTokenResult fetchRefreshTokenResult
							= fetchRefreshTokenSynchronous(context, redirectUri);

					if(fetchRefreshTokenResult.status
							!= FetchRefreshTokenResultStatus.SUCCESS) {

						listener.onLoginFailure(
								LoginError.fromFetchRefreshTokenStatus(
										fetchRefreshTokenResult.status),
								fetchRefreshTokenResult.error);

						return;
					}

					final FetchUserInfoResult fetchUserInfoResult
							= fetchUserInfoSynchronous(
							context,
							fetchRefreshTokenResult.accessToken);

					if(fetchUserInfoResult.status != FetchUserInfoResultStatus.SUCCESS) {
						listener.onLoginFailure(
								LoginError.fromFetchUserInfoStatus(fetchUserInfoResult.status),
								fetchUserInfoResult.error);

						return;
					}

					final RedditAccount account = new RedditAccount(
							fetchUserInfoResult.username,
							fetchRefreshTokenResult.refreshToken,
							true,
							0);

					account.setAccessToken(fetchRefreshTokenResult.accessToken);

					final RedditAccountManager accountManager =
							RedditAccountManager.getInstance(context);
					accountManager.addAccount(account);
					accountManager.setDefaultAccount(account);

					listener.onLoginSuccess(account);

				} catch(final Throwable t) {
					listener.onLoginFailure(
							LoginError.UNKNOWN_ERROR,
							new RRError(
									context.getString(R.string.error_unknown_title),
									context.getString(R.string.error_unknown_message),
									true,
									t
							)
					);
				}
			}
		}.start();
	}

	public enum FetchAccessTokenResultStatus {
		SUCCESS,
		INVALID_REQUEST,
		INVALID_RESPONSE,
		CONNECTION_ERROR,
		UNKNOWN_ERROR
	}

	public static final class FetchAccessTokenResult {

		public final FetchAccessTokenResultStatus status;
		public final RRError error;

		public final AccessToken accessToken;

		public FetchAccessTokenResult(
				final FetchAccessTokenResultStatus status,
				final RRError error) {
			this.status = status;
			this.error = error;
			this.accessToken = null;
		}

		public FetchAccessTokenResult(final AccessToken accessToken) {
			this.status = FetchAccessTokenResultStatus.SUCCESS;
			this.error = null;
			this.accessToken = accessToken;
		}
	}

	public static FetchAccessTokenResult fetchAccessTokenSynchronous(
			final Context context,
			final RedditAccount user) {

		final String uri = ACCESS_TOKEN_URL;

		final ArrayList<PostField> postFields = new ArrayList<>(2);
		postFields.add(new PostField("grant_type", "refresh_token"));
		postFields.add(new PostField("refresh_token", user.refreshToken.token));

		try {
			final HTTPBackend.Request request = HTTPBackend.getBackend()
					.prepareRequest(
							context,
							new HTTPBackend.RequestDetails(
									General.uriFromString(uri),
									Optional.of(new HTTPRequestBodyPostFields(postFields))));

			request.addHeader(
					"Authorization",
					"Basic " + Base64.encodeToString(
							((user.usesNewClientId ? CLIENT_ID_NEW : CLIENT_ID_OLD) + ":")
									.getBytes(),
							Base64.URL_SAFE | Base64.NO_WRAP));

			final AtomicReference<FetchAccessTokenResult> result =
					new AtomicReference<>();

			request.executeInThisThread(new HTTPBackend.Listener() {
				@Override
				public void onError(
						final @CacheRequest.RequestFailureType int failureType,
						final Throwable exception,
						final Integer httpStatus,
						@NonNull final Optional<FailedRequestBody> body) {
					result.set(handleAccessTokenError(
							exception,
							httpStatus,
							context,
							uri));
				}

				@Override
				public void onSuccess(
						final String mimetype,
						final Long bodyBytes,
						final InputStream body) {

					try {
						final JsonValue jsonValue = JsonValue.parse(body);
						final JsonObject responseObject = jsonValue.asObject();

						final String accessTokenString =
								responseObject.getString("access_token");

						if(accessTokenString == null) {
							throw new RuntimeException("Null access token: "
									+ responseObject.getString("error"));
						}

						final AccessToken accessToken =
								new AccessToken(accessTokenString);

						result.set(new FetchAccessTokenResult(accessToken));

					} catch(final IOException e) {
						result.set(new FetchAccessTokenResult(
								FetchAccessTokenResultStatus.CONNECTION_ERROR,
								new RRError(
										context.getString(R.string.error_connection_title),
										context.getString(R.string.error_connection_message),
										true,
										e,
										null,
										uri,
										null)
						));

					} catch(final Throwable t) {
						throw new RuntimeException(t);

					} finally {
						General.closeSafely(body);
					}
				}
			});

			return result.get();

		} catch(final Throwable t) {
			return new FetchAccessTokenResult(
					FetchAccessTokenResultStatus.UNKNOWN_ERROR,
					new RRError(
							context.getString(R.string.error_unknown_title),
							context.getString(R.string.error_unknown_message),
							true,
							t,
							null,
							uri,
							null)
			);
		}
	}

	public static FetchAccessTokenResult fetchAnonymousAccessTokenSynchronous(
			final Context context) {

		final String uri = ACCESS_TOKEN_URL;

		final ArrayList<PostField> postFields = new ArrayList<>(2);
		postFields.add(new PostField(
				"grant_type",
				"https://oauth.reddit.com/grants/installed_client"));
		postFields.add(new PostField(
				"device_id",
				"DO_NOT_TRACK_THIS_DEVICE"));

		try {
			final HTTPBackend.Request request = HTTPBackend.getBackend()
					.prepareRequest(
							context,
							new HTTPBackend.RequestDetails(
									General.uriFromString(uri),
									Optional.of(new HTTPRequestBodyPostFields(postFields))));

			request.addHeader(
					"Authorization",
					"Basic " + Base64.encodeToString(
							(CLIENT_ID_NEW + ":").getBytes(),
							Base64.URL_SAFE | Base64.NO_WRAP));

			final AtomicReference<FetchAccessTokenResult> result =
					new AtomicReference<>();

			request.executeInThisThread(new HTTPBackend.Listener() {
				@Override
				public void onError(
						final @CacheRequest.RequestFailureType int failureType,
						final Throwable exception,
						final Integer httpStatus,
						@NonNull final Optional<FailedRequestBody> body) {
					result.set(handleAccessTokenError(
							exception,
							httpStatus,
							context,
							uri));
				}

				@Override
				public void onSuccess(
						final String mimetype,
						final Long bodyBytes,
						final InputStream body) {

					try {
						final JsonValue jsonValue = JsonValue.parse(body);
						final JsonObject responseObject = jsonValue.asObject();

						final String accessTokenString =
								responseObject.getString("access_token");

						if(accessTokenString == null) {
							throw new RuntimeException("Null access token: "
									+ responseObject.getString("error"));
						}

						final AccessToken accessToken =
								new AccessToken(accessTokenString);

						result.set(new FetchAccessTokenResult(accessToken));

					} catch(final IOException e) {
						result.set(new FetchAccessTokenResult(
								FetchAccessTokenResultStatus.CONNECTION_ERROR,
								new RRError(
										context.getString(R.string.error_connection_title),
										context.getString(R.string.error_connection_message),
										true,
										e,
										null,
										uri,
										null)
						));

					} catch(final Throwable t) {
						throw new RuntimeException(t);

					} finally {
						General.closeSafely(body);
					}
				}
			});

			return result.get();

		} catch(final Throwable t) {
			return new FetchAccessTokenResult(
					FetchAccessTokenResultStatus.UNKNOWN_ERROR,
					new RRError(
							context.getString(R.string.error_unknown_title),
							context.getString(R.string.message_cannotlogin),
							true,
							t,
							null,
							uri,
							null)
			);
		}
	}

	public static void completeLogin(
			final AppCompatActivity activity,
			final Uri uri,
			final RunnableOnce onDone) {

		final ProgressDialog progressDialog = new ProgressDialog(activity);
		progressDialog.setTitle(R.string.accounts_loggingin);
		progressDialog.setMessage(activity.getApplicationContext().getString(
				R.string.accounts_loggingin_msg));
		progressDialog.setIndeterminate(true);
		progressDialog.setCancelable(true);
		progressDialog.setCanceledOnTouchOutside(false);

		final AtomicBoolean cancelled = new AtomicBoolean(false);

		progressDialog.setOnCancelListener(dialogInterface -> {

			if(!cancelled.getAndSet(true)) {
				General.safeDismissDialog(progressDialog);
				onDone.run();
			}
		});

		progressDialog.setOnKeyListener((dialogInterface, keyCode, keyEvent) -> {

			if(keyCode == KeyEvent.KEYCODE_BACK) {
				if(!cancelled.getAndSet(true)) {
					General.safeDismissDialog(progressDialog);
					onDone.run();
				}
			}

			return true;
		});

		progressDialog.show();

		RedditOAuth.loginAsynchronous(
				activity.getApplicationContext(),
				uri,

				new RedditOAuth.LoginListener() {
					@Override
					public void onLoginSuccess(final RedditAccount account) {
						AndroidCommon.UI_THREAD_HANDLER.post(() -> {

							if(cancelled.get()) {
								return;
							}

							General.safeDismissDialog(progressDialog);

							final AlertDialog.Builder alertBuilder
									= new AlertDialog.Builder(activity);

							alertBuilder.setNeutralButton(
									R.string.dialog_close,
									(dialog, which) -> onDone.run());

							alertBuilder.setOnCancelListener(dialog -> onDone.run());

							if(Build.VERSION.SDK_INT >= 17) {
								alertBuilder.setOnDismissListener(dialog -> onDone.run());
							}

							final Context context = activity.getApplicationContext();

							alertBuilder.setTitle(
									context.getString(R.string.general_success));

							alertBuilder.setMessage(
									context.getString(R.string.message_nowloggedin));

							alertBuilder.show();
						});
					}

					@Override
					public void onLoginFailure(
							final RedditOAuth.LoginError error,
							final RRError details) {

						AndroidCommon.UI_THREAD_HANDLER.post(() -> {

							if(cancelled.get()) {
								return;
							}

							General.safeDismissDialog(progressDialog);

							final AlertDialog.Builder builder = new AlertDialog.Builder(activity);

							builder.setNeutralButton(
									R.string.dialog_close,
									(dialog, which) -> onDone.run());

							builder.setOnCancelListener(dialog -> onDone.run());

							if(Build.VERSION.SDK_INT >= 17) {
								builder.setOnDismissListener(dialog -> onDone.run());
							}

							builder.setTitle(details.title);
							builder.setMessage(details.message);
							builder.show();
						});
					}
				});
	}
}
