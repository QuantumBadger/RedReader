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

import android.accounts.*;
import android.app.Activity;
import android.content.Context;
import android.os.Bundle;

import org.quantumbadger.redreader.cache.PersistentCookieStore;
import org.quantumbadger.redreader.common.UpdateNotifier;

import java.util.ArrayList;
import java.util.LinkedList;

public final class RedditAccountManager {

	private LinkedList<RedditAccount> accountsCache = null;
	private RedditAccount defaultAccountCache = null;

    private static final String USERDATA_PRIORITY = "priority";
    private static final String USERDATA_COOKIES = "cookies";

	private static final RedditAccount ANON = new RedditAccount("", null, null, 10);

    private final AccountManager mAccountManager;

	private final UpdateNotifier<RedditAccountChangeListener> updateNotifier = new UpdateNotifier<RedditAccountChangeListener>() {
		@Override
		protected void notifyListener(final RedditAccountChangeListener listener) {
			listener.onRedditAccountChanged();
		}
	};

	private static RedditAccountManager singleton;

	public static synchronized RedditAccountManager getInstance(final Context context) {
		if(singleton == null) singleton = new RedditAccountManager(context.getApplicationContext());
		return singleton;
	}

	public static synchronized RedditAccountManager getInstanceOrNull() {
		return singleton;
	}

	public static RedditAccount getAnon() {
		return ANON;
	}

	private RedditAccountManager(final Context context) {
        this.mAccountManager = AccountManager.get(context);
	}

	public synchronized void addAccount(final RedditAccount account) {

        final Account redditAccount = new Account(account.username, RedditAccountAuthenticator.ACCOUNT_TYPE);

        Bundle userdata = new Bundle();
        userdata.putString(USERDATA_PRIORITY, Long.toString(account.priority));
        userdata.putString(USERDATA_COOKIES, account.getCookieString());

        mAccountManager.addAccountExplicitly(redditAccount, null, userdata);
        mAccountManager.setAuthToken(redditAccount, RedditAccountAuthenticator.TOKENTYPE_MODHASH, account.modhash);

        updateNotifier.updateAllListeners();
	}

	public synchronized ArrayList<RedditAccount> getAccounts() {

        if(accountsCache == null) {
            reloadAccounts(true);
        }

        return new ArrayList<RedditAccount>(accountsCache);
	}

	public RedditAccount getAccount(String username) {

		final ArrayList<RedditAccount> accounts = getAccounts();
		RedditAccount selectedAccount = null;

		for(RedditAccount account : accounts) {
			if(!account.isAnonymous() && account.username.equalsIgnoreCase(username)) {
				selectedAccount = account;
				break;
			}
		}

		return selectedAccount;
	}

    public RedditAccount getAccountRequireToken(String username, AccountManagerCallback<Bundle> callback) {
        RedditAccount account = getAccount(username);

        if (account.modhash != null)
            return account;
        else {
            mAccountManager.getAuthToken(new Account(account.username, RedditAccountAuthenticator.ACCOUNT_TYPE), RedditAccountAuthenticator.TOKENTYPE_MODHASH, null, true, callback, null);

            return null;
        }
    }

    public RedditAccount getAccountRequireToken(String username, AccountManagerCallback<Bundle> callback, Activity activity) {
        RedditAccount account = getAccount(username);

        if (account.modhash != null)
            return account;
        else {
            mAccountManager.getAuthToken(new Account(account.username, RedditAccountAuthenticator.ACCOUNT_TYPE), RedditAccountAuthenticator.TOKENTYPE_MODHASH, null, activity, callback, null);

            return null;
        }
    }

	public synchronized RedditAccount getDefaultAccount() {

        if(defaultAccountCache == null) {
            reloadAccounts(true);
        }

        return defaultAccountCache;
	}

    public synchronized RedditAccount getDefaultAccountRequireToken(AccountManagerCallback<Bundle> callback) {
        RedditAccount account = getDefaultAccount();

        if (account.modhash != null)
            return account;
        else {
            mAccountManager.getAuthToken(new Account(account.username, RedditAccountAuthenticator.ACCOUNT_TYPE), RedditAccountAuthenticator.TOKENTYPE_MODHASH, null, true, callback, null);

            return null;
        }
    }

    public synchronized RedditAccount getDefaultAccountRequireToken(AccountManagerCallback<Bundle> callback, Activity activity) {
        RedditAccount account = getDefaultAccount();

        if (account.modhash != null)
            return account;
        else {
            mAccountManager.getAuthToken(new Account(account.username, RedditAccountAuthenticator.ACCOUNT_TYPE), RedditAccountAuthenticator.TOKENTYPE_MODHASH, null, activity, callback, null);

            return null;
        }
    }

	public synchronized void setDefaultAccount(final RedditAccount newDefault) {

        Account updateAccount = new Account(newDefault.username, RedditAccountAuthenticator.ACCOUNT_TYPE);

        mAccountManager.setUserData(updateAccount, USERDATA_PRIORITY, Long.toString(getDefaultAccount().priority - 1));

        reloadAccounts(true);
        updateNotifier.updateAllListeners();
	}

    private synchronized void reloadAccounts(boolean keepModhashes) {

        LinkedList<RedditAccount> oldAccounts = accountsCache;
        accountsCache = new LinkedList<RedditAccount>();
        defaultAccountCache = null;

        Account[] accounts = mAccountManager.getAccountsByType(RedditAccountAuthenticator.ACCOUNT_TYPE);

        for (Account account : accounts) {
            String username = account.name;
            String modhash = null;

            for(RedditAccount oldAccount : oldAccounts) {
                if (oldAccount.equals(account) && keepModhashes) {
                    modhash = oldAccount.modhash;
                }
            }

            long priority = new Long(mAccountManager.getUserData(account, USERDATA_PRIORITY));
            String cookies = mAccountManager.getUserData(account, USERDATA_COOKIES);

            RedditAccount redditAccount = new RedditAccount(username, modhash, cookies == null ? null : new PersistentCookieStore(cookies), priority);
            accountsCache.add(redditAccount);

            if(defaultAccountCache == null || redditAccount.priority < defaultAccountCache.priority)
                defaultAccountCache = redditAccount;
        }
    }

	public void addUpdateListener(final RedditAccountChangeListener listener) {
		updateNotifier.addListener(listener);
	}

	public void deleteAccount(RedditAccount account) {

        Account deleteAccount = new Account(account.username, RedditAccountAuthenticator.ACCOUNT_TYPE);
        mAccountManager.removeAccount(deleteAccount, null, null);
        updateNotifier.updateAllListeners();
	}
}
