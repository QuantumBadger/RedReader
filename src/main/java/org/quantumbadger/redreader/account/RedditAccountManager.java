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
import android.content.Context;
import android.os.Bundle;

import org.holoeverywhere.preference.PreferenceManager;
import org.holoeverywhere.preference.SharedPreferences;
import org.quantumbadger.redreader.cache.PersistentCookieStore;
import org.quantumbadger.redreader.common.UpdateNotifier;

import java.util.ArrayList;
import java.util.LinkedList;

public final class RedditAccountManager {

	private LinkedList<RedditAccount> accountsCache = null;
	private RedditAccount defaultAccountCache = null;

    private static final String USERDATA_PRIORITY = "priority";
    private static final String USERDATA_MODHASH = "modhash";
    private static final String ANON_PRIORITY_PREF = "anonymous_account_priority";

	private static final RedditAccount ANON = new RedditAccount("", null, null, 10);

    private final AccountManager mAccountManager;
    private final Context mContext;

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
        this.mContext = context;
	}

	public synchronized void addAccount(final RedditAccount account) {

        final Account redditAccount = new Account(account.username, RedditAccountAuthenticator.ACCOUNT_TYPE);

        Bundle userdata = new Bundle();
        userdata.putString(USERDATA_PRIORITY, Long.toString(account.priority));
        userdata.putString(USERDATA_MODHASH, account.modhash);

        mAccountManager.addAccountExplicitly(redditAccount, null, userdata);
        mAccountManager.setAuthToken(redditAccount, RedditAccountAuthenticator.TOKENTYPE_COOKIE, account.getCookieString());

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

	public synchronized RedditAccount getDefaultAccount() {

        if(defaultAccountCache == null) {
            reloadAccounts(true);
        }

        return defaultAccountCache;
	}

	public synchronized void setDefaultAccount(final RedditAccount newDefault) {

        long newPriority = defaultAccountCache.priority - 1;

        if (!newDefault.isAnonymous()) {
            Account updateAccount = new Account(newDefault.username, RedditAccountAuthenticator.ACCOUNT_TYPE);

            mAccountManager.setUserData(updateAccount, USERDATA_PRIORITY, Long.toString(newPriority));

            reloadAccounts(true);
        }
        else {
            SharedPreferences.Editor editor = PreferenceManager.getDefaultSharedPreferences(mContext).edit();
            editor.putLong(ANON_PRIORITY_PREF, newPriority);
            editor.commit();

            defaultAccountCache = new RedditAccount(newDefault.username, newDefault.modhash, newDefault.getCookies(), newPriority);
        }

        updateNotifier.updateAllListeners();
	}

    private synchronized void reloadAccounts(boolean keepCookies) {

        LinkedList<RedditAccount> oldAccounts = accountsCache;
        accountsCache = new LinkedList<RedditAccount>();
        defaultAccountCache = null;

        Account[] accounts = mAccountManager.getAccountsByType(RedditAccountAuthenticator.ACCOUNT_TYPE);

        long anonymousAccountPriority = PreferenceManager.getDefaultSharedPreferences(mContext).getLong(ANON_PRIORITY_PREF, 10);
        RedditAccount anon = getAnon();
        RedditAccount priorityAnon = new RedditAccount(anon.username, anon.modhash, anon.getCookies(), anonymousAccountPriority);
        accountsCache.add(priorityAnon);
        defaultAccountCache = priorityAnon;

        for (Account account : accounts) {
            String username = account.name;
            String cookies = null;

            //This is not save to call!
            //It will return NULL, if no Cookie is stored in the AccountManager
            cookies = mAccountManager.peekAuthToken(account, RedditAccountAuthenticator.TOKENTYPE_COOKIE);

            if (oldAccounts != null) {
                for(RedditAccount oldAccount : oldAccounts) {
                    if (oldAccount.equals(account) && keepCookies) {
                        cookies = oldAccount.getCookieString();
                    }
                }
            }

            long priority = new Long(mAccountManager.getUserData(account, USERDATA_PRIORITY));
            String modhash = mAccountManager.getUserData(account, USERDATA_MODHASH);

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
