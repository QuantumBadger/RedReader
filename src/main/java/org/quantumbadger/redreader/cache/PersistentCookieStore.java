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

import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.CookieStore;
import org.apache.http.cookie.Cookie;
import org.apache.http.impl.cookie.BasicClientCookie;

import java.util.*;
import java.util.regex.Pattern;

public final class PersistentCookieStore implements CookieStore {

	private LinkedList<Cookie> cookies = new LinkedList<Cookie>();

	public PersistentCookieStore() {}

	public PersistentCookieStore(final PersistentCookieStore existing) {
		this(existing.cookies);
	}

	private PersistentCookieStore(final Collection<Cookie> cookies) {
		this.cookies.addAll(cookies);
	}

    public PersistentCookieStore(final String data) throws ArrayIndexOutOfBoundsException{

        String[] cookieStrings = data.split(Pattern.quote("; "));
        Pattern cookieSplitter = Pattern.compile(",");

        for (String cookie : cookieStrings) {
            String[] cookieData = cookieSplitter.split(cookie);

            final String name = cookieData[0];
            final String value = cookieData[1];
            final String domain = cookieData[2];
            final String path = cookieData[3];
            final boolean isSecure = Boolean.valueOf(cookieData[4]);

            final BasicClientCookie result = new BasicClientCookie(name, value);
            result.setDomain(domain);
            result.setPath(path);
            result.setSecure(isSecure);

            cookies.add(result);
        }
    }

	public synchronized void addCookie(final Cookie cookie) {
		cookies.add(cookie);
	}

	public synchronized List<Cookie> getCookies() {
		return new ArrayList<Cookie>(cookies);
	}

	public synchronized boolean clearExpired(final Date date) {

		boolean purged = false;
		final LinkedList<Cookie> newSet = new LinkedList<Cookie>();

		for(final Cookie cookie : cookies) {

			if(!cookie.isExpired(date)) {
				newSet.add(cookie);
			} else {
				purged = true;
			}
		}

		cookies = newSet;
		return purged;
	}

	public synchronized void clear() {
		cookies = new LinkedList<Cookie>();
	}

	@Override
	public String toString() {

		final LinkedList<String> cookieStrings = new LinkedList<String>();

		for(final Cookie c : cookies) {
			cookieStrings.add(String.format("%s,%s,%s,%s, %b", c.getName(), c.getValue(), c.getDomain(), c.getPath(), c.isSecure()));
		}

		return StringUtils.join(cookieStrings, "; ");
	}
}
