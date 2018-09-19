package org.quantumbadger.redreader.account;

import org.junit.Test;

import static junit.framework.Assert.assertTrue;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.IsEqual.equalTo;
import static org.quantumbadger.redreader.account.AccountUtils.createAccount;


public class RedditAccountTest
{
	@Test( expected = RuntimeException.class )
	public void creatingAccountWithNullUserName_ThrowsException()
	{
		createAccount( null );
	}


	@Test
	public void userWithEmptyUserName_IsAnonymous()
	{
		RedditAccount user = createAccount( "" );

		assertTrue( user.isAnonymous() );
	}


	@Test
	public void canonicalUsername_TrimsAndLowercasesUsername()
	{
		RedditAccount user = createAccount( "  USER    " );
		String canonical = "user";

		assertThat( canonical, equalTo( user.getCanonicalUsername() ) );
	}


	@Test
	public void userEquals_returnsTrueIfTheSameUser()
	{
		RedditAccount user = createAccount( "dummy_user" );
		RedditAccount theSameUser = createAccount( "DUMMY_USER" );

		assertThat( user, equalTo( theSameUser ) );
	}


	@Test
	public void userEquals_returnsFalseIfOtherUser()
	{
		RedditAccount user = createAccount( "dummy_user" );
		RedditAccount otherUser = createAccount( "other_user" );
		Object otherObject = "dummy_user";

		assertThat( user, not( equalTo( otherUser ) ) );
		assertThat( user, not( equalTo( otherObject ) ) );
		assertThat( user, not( equalTo( null ) ) );
	}
}
