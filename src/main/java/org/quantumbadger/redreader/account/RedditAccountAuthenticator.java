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
import android.content.Intent;
import android.os.Bundle;
import org.quantumbadger.redreader.activities.RedditAuthenticatorActivity;

public class RedditAccountAuthenticator extends AbstractAccountAuthenticator{

    public final static String ACCOUNT_TYPE = "com.reddit";

    public final static String TOKENTYPE_MODHASH = "modhash";

    private final Context mContext;

    public RedditAccountAuthenticator(Context context) {
        super(context);

        this.mContext = context;
    }

    @Override
    public Bundle editProperties(AccountAuthenticatorResponse response, String accountType) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public Bundle addAccount(AccountAuthenticatorResponse response, String accountType, String authTokenType, String[] requiredFeatures, Bundle options) throws NetworkErrorException {
        final Intent intent = new Intent(mContext, RedditAuthenticatorActivity.class);
        intent.putExtra(RedditAuthenticatorActivity.ARG_ACCOUNT_TYPE, accountType);
        intent.putExtra(RedditAuthenticatorActivity.ARG_AUTH_TYPE, authTokenType);
        intent.putExtra(RedditAuthenticatorActivity.ARG_CREATE_ACCOUNT, true);
        intent.putExtra(AccountManager.KEY_ACCOUNT_AUTHENTICATOR_RESPONSE, response);

        final Bundle bundle = new Bundle();
        bundle.putParcelable(AccountManager.KEY_INTENT, intent);
        return bundle;
    }

    @Override
    public Bundle confirmCredentials(AccountAuthenticatorResponse response, Account account, Bundle options) throws NetworkErrorException {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public Bundle getAuthToken(AccountAuthenticatorResponse response, Account account, String authTokenType, Bundle options) throws NetworkErrorException {

        if (!authTokenType.equals(TOKENTYPE_MODHASH)) {
            final Bundle result = new Bundle();
            result.putString(AccountManager.KEY_ERROR_MESSAGE, "invalid authTokenType");
            return result;
        }

        final AccountManager accountManager = AccountManager.get(mContext);
        String authToken = accountManager.peekAuthToken(account, authTokenType);

        if (!authToken.isEmpty()) {
            final Bundle result = new Bundle();
            result.putString(AccountManager.KEY_ACCOUNT_NAME, account.name);
            result.putString(AccountManager.KEY_ACCOUNT_TYPE, account.type);
            result.putString(AccountManager.KEY_AUTHTOKEN, authToken);
            return result;
        }

        final Intent intent = new Intent(mContext, RedditAuthenticatorActivity.class);
        intent.putExtra(AccountManager.KEY_ACCOUNT_AUTHENTICATOR_RESPONSE, response);
        intent.putExtra(RedditAuthenticatorActivity.ARG_ACCOUNT_TYPE, account.type);
        intent.putExtra(RedditAuthenticatorActivity.ARG_AUTH_TYPE, authTokenType);
        intent.putExtra(RedditAuthenticatorActivity.ARG_ACCOUNT_NAME, account.name);
        final Bundle result = new Bundle();
        result.putParcelable(AccountManager.KEY_INTENT, intent);
        return result;
    }

    @Override
    public String getAuthTokenLabel(String authTokenType) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public Bundle updateCredentials(AccountAuthenticatorResponse response, Account account, String authTokenType, Bundle options) throws NetworkErrorException {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public Bundle hasFeatures(AccountAuthenticatorResponse accountAuthenticatorResponse, Account account, String[] features) throws NetworkErrorException {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
