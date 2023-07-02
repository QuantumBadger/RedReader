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

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.common.StringUtils;
import org.quantumbadger.redreader.reddit.api.RedditOAuth;

import java.util.Objects;

public class RedditAccount {

	@NonNull public final String username;
	public final RedditOAuth.RefreshToken refreshToken;

	private RedditOAuth.AccessToken accessToken;

	public final long priority;
	@Nullable public final String clientId;

	public RedditAccount(
			@NonNull final String username,
			final RedditOAuth.RefreshToken refreshToken,
			final long priority,
			@Nullable final String clientId) {

		//noinspection ConstantConditions
		if(username == null) {
			throw new RuntimeException("Null user in RedditAccount");
		}

		this.username = username.trim();
		this.refreshToken = refreshToken;
		this.priority = priority;
		this.clientId = clientId;
	}

	public boolean isAnonymous() {
		return username.isEmpty();
	}

	public boolean isNotAnonymous() {
		return !isAnonymous();
	}

	public String getCanonicalUsername() {
		return StringUtils.asciiLowercase(username.trim());
	}

	public synchronized RedditOAuth.AccessToken getMostRecentAccessToken() {
		return accessToken;
	}

	public synchronized void setAccessToken(final RedditOAuth.AccessToken token) {
		accessToken = token;
	}

	@Override
	public boolean equals(final Object o) {

		if(!(o instanceof RedditAccount)) {
			return false;
		}

		final RedditAccount other = (RedditAccount)o;

		return username.equalsIgnoreCase(other.username)
				&& Objects.equals(clientId, other.clientId)
				&& Objects.equals(refreshToken, other.refreshToken);
	}

	@Override
	public int hashCode() {
		return getCanonicalUsername().hashCode();
	}
}
