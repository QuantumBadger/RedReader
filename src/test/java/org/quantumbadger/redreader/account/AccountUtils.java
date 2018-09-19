package org.quantumbadger.redreader.account;

import java.util.ArrayList;
import java.util.List;


public class AccountUtils
{
	static RedditAccount createAccount( String username )
	{
		return new RedditAccount( username, null, 1 );
	}
	static List<RedditAccount> createAccounts( String ...usernames )
	{
		List<RedditAccount> accounts = new ArrayList<>(  );
		for( String username : usernames )
		{
			RedditAccount account = createAccount( username );
			accounts.add( account );
		}
		return accounts;
	}
}
