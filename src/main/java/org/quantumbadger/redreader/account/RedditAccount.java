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

package org.quantumbadger.redreader.account;

import android.content.Context;
import android.util.Log;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.params.ClientPNames;
import org.apache.http.client.protocol.ClientContext;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.CoreConnectionPNames;
import org.apache.http.params.CoreProtocolPNames;
import org.apache.http.params.HttpParams;
import org.apache.http.protocol.BasicHttpContext;
import org.apache.http.protocol.HTTP;
import org.apache.http.protocol.HttpContext;
import org.quantumbadger.redreader.cache.PersistentCookieStore;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedArray;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;

public class RedditAccount {

	public static enum LoginResult {
		SUCCESS, INTERNAL_ERROR, CONNECTION_ERROR, REQUEST_ERROR, JSON_ERROR, WRONG_PASSWORD, UNKNOWN_REDDIT_ERROR, RATELIMIT
	}

	public static class LoginResultPair {
		public final LoginResult result;
		public final RedditAccount account;
		public final String extraMessage;

		public LoginResultPair(final LoginResult result) {
			this(result, null, null);
		}

		public LoginResultPair(final LoginResult result, String extraMessage) {
			this(result, null, extraMessage);
		}

		public LoginResultPair(final LoginResult result, final RedditAccount account, final String extraMessage) {
			this.result = result;
			this.account = account;
			this.extraMessage = extraMessage;
		}
	}

	public final String username, modhash;
	private final PersistentCookieStore cookies;
	public final long priority;

	public RedditAccount(final String username, final String modhash, final PersistentCookieStore cookies, final long priority) {

		if(username == null) throw new RuntimeException("Null user in RedditAccount");

		this.username = username.trim();
		this.modhash = modhash;
		this.cookies = cookies;
		this.priority = priority;
	}

	public boolean isAnonymous() {
		return username.length() == 0;
	}

	public static LoginResultPair login(final Context context, final String username, final String password, final HttpClient client) {

		final ArrayList<NameValuePair> fields = new ArrayList<NameValuePair>(3);
		fields.add(new BasicNameValuePair("user", username));
		fields.add(new BasicNameValuePair("passwd", password));
		fields.add(new BasicNameValuePair("api_type", "json"));

		// TODO put somewhere else
		final HttpParams params = new BasicHttpParams();
		params.setParameter(CoreProtocolPNames.USER_AGENT, Constants.ua(context));
		params.setParameter(CoreConnectionPNames.SO_TIMEOUT, 20000); // TODO remove hardcoded params, put in network prefs
		params.setParameter(CoreConnectionPNames.CONNECTION_TIMEOUT, 20000);
		params.setParameter(CoreConnectionPNames.MAX_HEADER_COUNT,100);
		params.setParameter(ClientPNames.HANDLE_REDIRECTS, true);
		params.setParameter(ClientPNames.MAX_REDIRECTS, 5);

		final HttpPost request = new HttpPost("https://ssl.reddit.com/api/login");
		request.setParams(params);

		try {
			request.setEntity(new UrlEncodedFormEntity(fields, HTTP.UTF_8));
		} catch (UnsupportedEncodingException e) {
			return new LoginResultPair(LoginResult.INTERNAL_ERROR);
		}

		final PersistentCookieStore cookies = new PersistentCookieStore();

		final HttpContext localContext = new BasicHttpContext();
		localContext.setAttribute(ClientContext.COOKIE_STORE, cookies);

		final StatusLine status;
		final HttpEntity entity;
		try {
			final HttpResponse response = client.execute(request, localContext);
			status = response.getStatusLine();
			entity = response.getEntity();

		} catch (IOException e) {
			return new LoginResultPair(LoginResult.CONNECTION_ERROR);
		}

		if(status.getStatusCode() != 200) {
			return new LoginResultPair(LoginResult.REQUEST_ERROR);
		}

		final JsonValue result;

		try {
			result = new JsonValue(entity.getContent());
			result.buildInThisThread();
		} catch (IOException e) {
			return new LoginResultPair(LoginResult.CONNECTION_ERROR);
		}

		final String modhash;

		try {

			// TODO use the more general reddit error finder
			final JsonBufferedArray errors = result.asObject().getObject("json").getArray("errors");
			errors.join();
			if(errors.getCurrentItemCount() != 0) {

				for(final JsonValue v : errors) {
					for(final JsonValue s : v.asArray()) {

						// TODO handle unknown messages by concatenating all Reddit's strings

						if(s.getType() == JsonValue.Type.STRING) {

							Log.i("RR DEBUG ERROR", s.asString());

							// lol, reddit api
							if(s.asString().equalsIgnoreCase("WRONG_PASSWORD")
									|| s.asString().equalsIgnoreCase("invalid password")
									|| s.asString().equalsIgnoreCase("passwd"))
								return new LoginResultPair(LoginResult.WRONG_PASSWORD);

							if(s.asString().contains("too much")) // also "RATELIMIT", but that's not as descriptive
								return new LoginResultPair(LoginResult.RATELIMIT, s.asString());
						}
					}
				}

				return new LoginResultPair(LoginResult.UNKNOWN_REDDIT_ERROR);
			}

			final JsonBufferedObject data = result.asObject().getObject("json").getObject("data");

			modhash = data.getString("modhash");

		} catch(NullPointerException e) {
			return new LoginResultPair(LoginResult.JSON_ERROR);
		} catch (InterruptedException e) {
			return new LoginResultPair(LoginResult.JSON_ERROR);
		} catch (IOException e) {
			return new LoginResultPair(LoginResult.JSON_ERROR);
		}

		return new LoginResultPair(LoginResult.SUCCESS, new RedditAccount(username, modhash, cookies, 0), null);
	}

	public PersistentCookieStore getCookies() {
		if(cookies == null) return null;
		return new PersistentCookieStore(cookies);
	}

	public byte[] getCookieBytes() {
		if(cookies == null) return null;
		return cookies.toByteArray();
	}

	@Override
	public boolean equals(final Object o) {
		return o instanceof RedditAccount && username.equals(((RedditAccount) o).username);
	}
}
