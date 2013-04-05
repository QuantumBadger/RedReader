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

import java.io.*;
import java.util.*;

public final class PersistentCookieStore implements CookieStore {

	private LinkedList<Cookie> cookies = new LinkedList<Cookie>();

	public PersistentCookieStore() {}

	public PersistentCookieStore(final PersistentCookieStore existing) {
		this(existing.cookies);
	}

	private PersistentCookieStore(final Collection<Cookie> cookies) {
		this.cookies.addAll(cookies);
	}

	public PersistentCookieStore(final byte[] data) throws IOException {

		final DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));

		final int len = dis.readInt();

		for(int i = 0; i < len; i++) {

			final String name = dis.readUTF();
			final String value = dis.readUTF();
			final String domain = dis.readUTF();
			final String path = dis.readUTF();
			final boolean isSecure = dis.readBoolean();

			final BasicClientCookie cookie = new BasicClientCookie(name, value);
			cookie.setDomain(domain);
			cookie.setPath(path);
			cookie.setSecure(isSecure);

			cookies.add(cookie);
		}
	}

	public synchronized byte[] toByteArray() {

		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final DataOutputStream dos = new DataOutputStream(baos);

		try {

			dos.writeInt(cookies.size());

			for(final Cookie cookie : cookies) {

				dos.writeUTF(cookie.getName());
				dos.writeUTF(cookie.getValue());
				dos.writeUTF(cookie.getDomain());
				dos.writeUTF(cookie.getPath());
				dos.writeBoolean(cookie.isSecure());
			}

			dos.flush();
			dos.close();
		} catch(IOException e) {
			throw new RuntimeException(e);
		}

		return baos.toByteArray();
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
			cookieStrings.add(String.format("%s=%s", c.getName(), c.getValue()));
		}

		return StringUtils.join(cookieStrings, "; ");
	}
}
