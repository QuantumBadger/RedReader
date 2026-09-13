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

package org.quantumbadger.redreader.test.account;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

import org.junit.Test;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.reddit.api.RedditOAuth;

public class RedditAccountEqualityTest {

	@Test
	public void testRefreshTokenComparedByValue() {

		final RedditOAuth.RefreshToken a = new RedditOAuth.RefreshToken("token123");
		final RedditOAuth.RefreshToken b = new RedditOAuth.RefreshToken("token123");
		final RedditOAuth.RefreshToken c = new RedditOAuth.RefreshToken("other");

		assertEquals(a, b);
		assertEquals(a.hashCode(), b.hashCode());
		assertNotEquals(a, c);
	}

	@Test
	public void testAccountsReloadedFromDbAreEqual() {

		// RedditAccountManager.reloadAccounts() creates fresh RedditAccount and
		// RefreshToken instances on every reload. Two such generations for the
		// same account must compare equal, otherwise singletons keyed on the
		// account (e.g. RedditSubredditManager) get needlessly recreated.

		final RedditAccount gen1 = new RedditAccount(
				"SomeUser",
				new RedditOAuth.RefreshToken("token123"),
				0,
				"clientId");

		final RedditAccount gen2 = new RedditAccount(
				"SomeUser",
				new RedditOAuth.RefreshToken("token123"),
				5,
				"clientId");

		assertEquals(gen1, gen2);
		assertEquals(gen1.hashCode(), gen2.hashCode());
	}

	@Test
	public void testAccountsWithDifferentTokensAreNotEqual() {

		final RedditAccount before = new RedditAccount(
				"SomeUser",
				new RedditOAuth.RefreshToken("token123"),
				0,
				"clientId");

		final RedditAccount reauthenticated = new RedditAccount(
				"SomeUser",
				new RedditOAuth.RefreshToken("token456"),
				0,
				"clientId");

		assertNotEquals(before, reauthenticated);
	}

	@Test
	public void testAnonymousAccountsAreEqual() {

		final RedditAccount anon1 = new RedditAccount("", null, 0, null);
		final RedditAccount anon2 = new RedditAccount("", null, 0, null);

		assertEquals(anon1, anon2);
	}
}
