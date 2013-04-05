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

package org.quantumbadger.redreader.cache;

import org.quantumbadger.redreader.account.RedditAccount;

import java.net.URI;
import java.util.UUID;

public final class RequestIdentifier {

	private final URI url;
	private final String user;
	private final UUID session;
	private final boolean unique;

	public RequestIdentifier(final URI url, final RedditAccount user, final UUID session, final boolean unique) {

		if(url == null) throw new NullPointerException("URL must not be null");
		if(user == null) throw new NullPointerException("User must not be null");

		this.url = url;
		this.user = user.username;
		this.session = session;
		this.unique = unique;
	}

	@Override
	public boolean equals(final Object o) {

		if(this == o) return true;

		if(o == null) return false;
		if(!(o instanceof RequestIdentifier)) return false;

		final RequestIdentifier other = (RequestIdentifier)o;

		if(!unique || !other.unique) return false;

		if(!other.url.equals(url)) return false;
		if(!other.user.equals(user)) return false;

		return other.session == session || other.session.equals(session);

	}

	@Override
	public int hashCode() {
		return url.hashCode() ^ (session == null ? 0 : session.hashCode()) ^ user.hashCode();
	}

	public URI getUrl() {
		return url;
	}
}
