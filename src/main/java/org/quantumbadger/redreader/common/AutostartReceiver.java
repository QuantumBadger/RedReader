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

package org.quantumbadger.redreader.common;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.content.BroadcastReceiver;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;

public class AutostartReceiver extends BroadcastReceiver {

    private static final String ACCOUNT_TYPE = "com.reddit";
    private static final String SYNC_AUTHORITY = "org.quantumbadger.redreader.provider";

    @Override
    public void onReceive(Context context, Intent intent) {
        if (intent.getAction().equals("android.intent.action.BOOT_COMPLETED")) {
            Log.d("RedReader", "Autostart init");

            AccountManager manager = AccountManager.get(context);
            Account[] accounts = manager.getAccountsByType(ACCOUNT_TYPE);

            //TODO Make interval a user preference
            int syncInterval = 15 * 60;

            for (Account account : accounts) {
                ContentResolver.addPeriodicSync(account, SYNC_AUTHORITY, new Bundle(), syncInterval);
            }
        }
    }
}
