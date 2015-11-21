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

import android.content.Context;
import android.net.Uri;
import android.os.SystemClock;
import android.util.Base64;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.message.BasicNameValuePair;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;

public final class RedditOAuth {

	private static final String REDIRECT_URI = "http://rr_oauth_redir";
	private static final String CLIENT_ID = "m_zCW1Dixs9WLA";
	private static final String ALL_SCOPES
			= "identity,edit,flair,history,modconfig,modflair,modlog,modposts,modwiki,mysubreddits,privatemessages,"
					+ "read,report,save,submit,subscribe,vote,wikiedit,wikiread";

	private static final String ACCESS_TOKEN_URL = "https://www.reddit.com/api/v1/access_token";

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

		public FetchRefreshTokenResult(final FetchRefreshTokenResultStatus status, final RRError error) {
			this.status = status;
			this.error = error;
			this.refreshToken = null;
			this.accessToken = null;
		}

		public FetchRefreshTokenResult(final RefreshToken refreshToken, final AccessToken accessToken) {
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

		public FetchUserInfoResult(final FetchUserInfoResultStatus status, final RRError error) {
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

		final Uri.Builder uri = Uri.parse("https://www.reddit.com/api/v1/authorize.compact").buildUpon();

		uri.appendQueryParameter("response_type", "code");
		uri.appendQueryParameter("duration", "permanent");
		uri.appendQueryParameter("state", "Texas");
		uri.appendQueryParameter("redirect_uri", REDIRECT_URI);
		uri.appendQueryParameter("client_id", CLIENT_ID);
		uri.appendQueryParameter("scope", ALL_SCOPES);

		return uri.build();
	}

	private static FetchRefreshTokenResult fetchRefreshTokenSynchronous(final Context context, final Uri redirectUri) {

		final String error = redirectUri.getQueryParameter("error");

		if(error != null) {

			if(error.equals("access_denied")) {
				return new FetchRefreshTokenResult(
						FetchRefreshTokenResultStatus.USER_REFUSED_PERMISSION,
						new RRError(
								context.getString(R.string.error_title_login_user_denied_permission),
								context.getString(R.string.error_message_login_user_denied_permission)
						)
				);

			} else {
				return new FetchRefreshTokenResult(
						FetchRefreshTokenResultStatus.INVALID_REQUEST,
						new RRError(
								context.getString(R.string.error_title_login_unknown_reddit_error) + error,
								context.getString(R.string.error_unknown_message)
						));
			}
		}

		final String code = redirectUri.getQueryParameter("code");

		if(code == null) {
			return new FetchRefreshTokenResult(
					FetchRefreshTokenResultStatus.INVALID_RESPONSE,
					new RRError(
							context.getString(R.string.error_unknown_title),
							context.getString(R.string.error_unknown_message)
					)
			);
		}

		final String uri = ACCESS_TOKEN_URL;
		StatusLine responseStatus = null;

		try {
			final HttpClient httpClient = CacheManager.createHttpClient(context);

			final HttpPost request = new HttpPost(uri);

			final ArrayList<NameValuePair> nameValuePairs = new ArrayList<NameValuePair>(3);
			nameValuePairs.add(new BasicNameValuePair("grant_type", "authorization_code"));
			nameValuePairs.add(new BasicNameValuePair("code", code));
			nameValuePairs.add(new BasicNameValuePair("redirect_uri", REDIRECT_URI));
			request.setEntity(new UrlEncodedFormEntity(nameValuePairs));

			request.addHeader("Authorization", "Basic " + Base64.encodeToString((CLIENT_ID + ":").getBytes(), Base64.URL_SAFE | Base64.NO_WRAP));

			final HttpResponse response = httpClient.execute(request);
			responseStatus = response.getStatusLine();

			if(responseStatus.getStatusCode() != 200) {
				return new FetchRefreshTokenResult(
						FetchRefreshTokenResultStatus.UNKNOWN_ERROR,
						new RRError(
								context.getString(R.string.error_unknown_title),
								context.getString(R.string.message_cannotlogin),
								null,
								responseStatus.getStatusCode(),
								request.getURI().toString()
						)
				);
			}

			final JsonValue jsonValue = new JsonValue(response.getEntity().getContent());
			jsonValue.buildInThisThread();
			final JsonBufferedObject responseObject = jsonValue.asObject();

			final RefreshToken refreshToken = new RefreshToken(responseObject.getString("refresh_token"));
			final AccessToken accessToken = new AccessToken(responseObject.getString("access_token"));

			return new FetchRefreshTokenResult(refreshToken, accessToken);

		} catch(IOException e) {
			return new FetchRefreshTokenResult(
					FetchRefreshTokenResultStatus.CONNECTION_ERROR,
					new RRError(
							context.getString(R.string.error_connection_title),
							context.getString(R.string.error_connection_message),
							e,
							responseStatus.getStatusCode(),
							uri
					)
			);

		} catch(Throwable t) {
			return new FetchRefreshTokenResult(
					FetchRefreshTokenResultStatus.UNKNOWN_ERROR,
					new RRError(
							context.getString(R.string.error_unknown_title),
							context.getString(R.string.error_unknown_message),
							t,
							responseStatus.getStatusCode(),
							uri
					)
			);
		}
	}

	private static FetchUserInfoResult fetchUserInfoSynchronous(final Context context, final AccessToken accessToken) {

		final URI uri = Constants.Reddit.getUri(Constants.Reddit.PATH_ME);
		StatusLine responseStatus = null;

		try {
			final HttpClient httpClient = CacheManager.createHttpClient(context);

			final HttpGet request = new HttpGet(uri);
			request.addHeader("Authorization", "bearer " + accessToken.token);

			final HttpResponse response = httpClient.execute(request);
			responseStatus = response.getStatusLine();

			if(responseStatus.getStatusCode() != 200) {
				return new FetchUserInfoResult(
						FetchUserInfoResultStatus.CONNECTION_ERROR,
						new RRError(
								context.getString(R.string.error_unknown_title),
								context.getString(R.string.error_unknown_message),
								null,
								responseStatus.getStatusCode(),
								uri.toString()
						)
				);
			}

			final JsonValue jsonValue = new JsonValue(response.getEntity().getContent());
			jsonValue.buildInThisThread();
			final JsonBufferedObject responseObject = jsonValue.asObject();

			final String username = responseObject.getString("name");

			if(username == null || username.length() == 0) {
				return new FetchUserInfoResult(
						FetchUserInfoResultStatus.INVALID_RESPONSE,
						new RRError(
								context.getString(R.string.error_unknown_title),
								context.getString(R.string.error_unknown_message),
								null,
								responseStatus.getStatusCode(),
								uri.toString()
						)
				);
			}

			return new FetchUserInfoResult(username);

		} catch(IOException e) {
			return new FetchUserInfoResult(
					FetchUserInfoResultStatus.CONNECTION_ERROR,
					new RRError(
							context.getString(R.string.error_connection_title),
							context.getString(R.string.error_connection_message),
							e,
							responseStatus != null ? responseStatus.getStatusCode() : null,
							uri.toString()
					)
			);

		} catch(Throwable t) {
			return new FetchUserInfoResult(
					FetchUserInfoResultStatus.UNKNOWN_ERROR,
					new RRError(
							context.getString(R.string.error_unknown_title),
							context.getString(R.string.error_unknown_message),
							t,
							responseStatus != null ? responseStatus.getStatusCode() : null,
							uri.toString()
					)
			);
		}
	}

	public enum LoginError {
		SUCCESS,
		USER_REFUSED_PERMISSION,
		CONNECTION_ERROR,
		UNKNOWN_ERROR;

		static LoginError fromFetchRefreshTokenStatus(FetchRefreshTokenResultStatus status) {
			switch(status) {
				case SUCCESS:					return SUCCESS;
				case USER_REFUSED_PERMISSION:	return USER_REFUSED_PERMISSION;
				case INVALID_REQUEST:			return UNKNOWN_ERROR;
				case INVALID_RESPONSE:			return UNKNOWN_ERROR;
				case CONNECTION_ERROR:			return CONNECTION_ERROR;
				case UNKNOWN_ERROR:				return UNKNOWN_ERROR;
			}

			return UNKNOWN_ERROR;
		}

		static LoginError fromFetchUserInfoStatus(FetchUserInfoResultStatus status) {
			switch(status) {
				case SUCCESS:					return SUCCESS;
				case INVALID_RESPONSE:			return UNKNOWN_ERROR;
				case CONNECTION_ERROR:			return CONNECTION_ERROR;
				case UNKNOWN_ERROR:				return UNKNOWN_ERROR;
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

					if(fetchRefreshTokenResult.status != FetchRefreshTokenResultStatus.SUCCESS) {

						listener.onLoginFailure(
								LoginError.fromFetchRefreshTokenStatus(fetchRefreshTokenResult.status),
								fetchRefreshTokenResult.error);

						return;
					}

					final FetchUserInfoResult fetchUserInfoResult
							= fetchUserInfoSynchronous(context, fetchRefreshTokenResult.accessToken);

					if(fetchUserInfoResult.status != FetchUserInfoResultStatus.SUCCESS) {
						listener.onLoginFailure(
								LoginError.fromFetchUserInfoStatus(fetchUserInfoResult.status),
								fetchUserInfoResult.error);

						return;
					}

					final RedditAccount account = new RedditAccount(
							fetchUserInfoResult.username,
							fetchRefreshTokenResult.refreshToken,
							0);

					account.setAccessToken(fetchRefreshTokenResult.accessToken);

					final RedditAccountManager accountManager = RedditAccountManager.getInstance(context);
					accountManager.addAccount(account);
					accountManager.setDefaultAccount(account);

					listener.onLoginSuccess(account);

				} catch(Throwable t) {
					listener.onLoginFailure(
							LoginError.UNKNOWN_ERROR,
							new RRError(
									context.getString(R.string.error_unknown_title),
									context.getString(R.string.error_unknown_message),
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

		public FetchAccessTokenResult(final FetchAccessTokenResultStatus status, final RRError error) {
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

	public static FetchAccessTokenResult fetchAccessTokenSynchronous(final Context context, final RefreshToken refreshToken) {

		final String uri = ACCESS_TOKEN_URL;
		StatusLine responseStatus = null;

		try {
			final HttpClient httpClient = CacheManager.createHttpClient(context);

			final HttpPost request = new HttpPost(uri);

			final ArrayList<NameValuePair> nameValuePairs = new ArrayList<NameValuePair>(2);
			nameValuePairs.add(new BasicNameValuePair("grant_type", "refresh_token"));
			nameValuePairs.add(new BasicNameValuePair("refresh_token", refreshToken.token));
			request.setEntity(new UrlEncodedFormEntity(nameValuePairs));

			request.addHeader("Authorization", "Basic " + Base64.encodeToString((CLIENT_ID + ":").getBytes(), Base64.URL_SAFE | Base64.NO_WRAP));

			final HttpResponse response = httpClient.execute(request);
			responseStatus = response.getStatusLine();

			if(responseStatus.getStatusCode() != 200) {
				return new FetchAccessTokenResult(
						FetchAccessTokenResultStatus.UNKNOWN_ERROR,
						new RRError(
								context.getString(R.string.error_unknown_title),
								context.getString(R.string.message_cannotlogin),
								null,
								responseStatus.getStatusCode(),
								request.getURI().toString()
						)
				);
			}

			final JsonValue jsonValue = new JsonValue(response.getEntity().getContent());
			jsonValue.buildInThisThread();
			final JsonBufferedObject responseObject = jsonValue.asObject();

			final String accessTokenString = responseObject.getString("access_token");

			if(accessTokenString == null) {
				throw new RuntimeException("Null access token: " + responseObject.getString("error"));
			}

			final AccessToken accessToken = new AccessToken(accessTokenString);

			return new FetchAccessTokenResult(accessToken);

		} catch(IOException e) {
			return new FetchAccessTokenResult(
					FetchAccessTokenResultStatus.CONNECTION_ERROR,
					new RRError(
							context.getString(R.string.error_connection_title),
							context.getString(R.string.error_connection_message),
							e,
							responseStatus != null ? responseStatus.getStatusCode() : null,
							uri
					)
			);

		} catch(Throwable t) {
			return new FetchAccessTokenResult(
					FetchAccessTokenResultStatus.UNKNOWN_ERROR,
					new RRError(
							context.getString(R.string.error_unknown_title),
							context.getString(R.string.error_unknown_message),
							t,
							responseStatus != null ? responseStatus.getStatusCode() : null,
							uri
					)
			);
		}
	}

	public static FetchAccessTokenResult fetchAnonymousAccessTokenSynchronous(final Context context) {

		final String uri = ACCESS_TOKEN_URL;
		StatusLine responseStatus = null;

		try {
			final HttpClient httpClient = CacheManager.createHttpClient(context);

			final HttpPost request = new HttpPost(uri);

			final ArrayList<NameValuePair> nameValuePairs = new ArrayList<NameValuePair>(2);
			nameValuePairs.add(new BasicNameValuePair("grant_type", "https://oauth.reddit.com/grants/installed_client"));
			nameValuePairs.add(new BasicNameValuePair("device_id", "DO_NOT_TRACK_THIS_DEVICE"));
			request.setEntity(new UrlEncodedFormEntity(nameValuePairs));

			request.addHeader("Authorization", "Basic " + Base64.encodeToString((CLIENT_ID + ":").getBytes(), Base64.URL_SAFE | Base64.NO_WRAP));

			final HttpResponse response = httpClient.execute(request);
			responseStatus = response.getStatusLine();

			if(responseStatus.getStatusCode() != 200) {
				return new FetchAccessTokenResult(
						FetchAccessTokenResultStatus.UNKNOWN_ERROR,
						new RRError(
								context.getString(R.string.error_unknown_title),
								context.getString(R.string.message_cannotlogin),
								null,
								responseStatus.getStatusCode(),
								request.getURI().toString()
						)
				);
			}

			final JsonValue jsonValue = new JsonValue(response.getEntity().getContent());
			jsonValue.buildInThisThread();
			final JsonBufferedObject responseObject = jsonValue.asObject();

			final String accessTokenString = responseObject.getString("access_token");

			if(accessTokenString == null) {
				throw new RuntimeException("Null access token: " + responseObject.getString("error"));
			}

			final AccessToken accessToken = new AccessToken(accessTokenString);

			return new FetchAccessTokenResult(accessToken);

		} catch(IOException e) {
			return new FetchAccessTokenResult(
					FetchAccessTokenResultStatus.CONNECTION_ERROR,
					new RRError(
							context.getString(R.string.error_connection_title),
							context.getString(R.string.error_connection_message),
							e,
							responseStatus != null ? responseStatus.getStatusCode() : null,
							uri
					)
			);

		} catch(Throwable t) {
			return new FetchAccessTokenResult(
					FetchAccessTokenResultStatus.UNKNOWN_ERROR,
					new RRError(
							context.getString(R.string.error_unknown_title),
							context.getString(R.string.message_cannotlogin),
							t,
							responseStatus != null ? responseStatus.getStatusCode() : null,
							uri
					)
			);
		}
	}
}
