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

package org.quantumbadger.redreader.reddit.api;

import android.content.Context;
import android.util.Log;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.common.TimestampBound;
import org.quantumbadger.redreader.common.UnexpectedInternalStateException;
import org.quantumbadger.redreader.common.collections.WeakReferenceListManager;
import org.quantumbadger.redreader.io.RawObjectDB;
import org.quantumbadger.redreader.io.RequestResponseHandler;
import org.quantumbadger.redreader.io.WritableHashSet;
import org.quantumbadger.redreader.reddit.RedditSubredditManager;

import java.util.ArrayList;
import java.util.HashSet;

public class RedditSubredditSubscriptionManager {

	static enum SubscriptionState { SUBSCRIBED, SUBSCRIBING, UNSUBSCRIBING, NOT_SUBSCRIBED }

	private final SubredditSubscriptionStateChangeNotifier notifier = new SubredditSubscriptionStateChangeNotifier();
	private final WeakReferenceListManager<SubredditSubscriptionStateChangeListener> listeners
			= new WeakReferenceListManager<SubredditSubscriptionStateChangeListener>();

	private static RedditSubredditSubscriptionManager singleton;
	private static RedditAccount singletonAccount;

	private final RedditAccount user;
	private final Context context;

	private static RawObjectDB<String, WritableHashSet> db = null;

	private WritableHashSet subscriptions;
	private HashSet<String> pendingSubscriptions = new HashSet<String>(), pendingUnsubscriptions = new HashSet<String>();

	public static synchronized RedditSubredditSubscriptionManager getSingleton(final Context context, final RedditAccount account) {

		if(db == null) {
			db = new RawObjectDB<String, WritableHashSet>(context, "rr_subscriptions.db", WritableHashSet.class);
		}

		if(singleton == null || !account.equals(RedditSubredditSubscriptionManager.singletonAccount)) {
			singleton = new RedditSubredditSubscriptionManager(account, context);
			RedditSubredditSubscriptionManager.singletonAccount = account;
		}

		return singleton;
	}

	private RedditSubredditSubscriptionManager(RedditAccount user, Context context) {

		this.user = user;
		this.context = context;

		subscriptions = db.getById(user.getCanonicalUsername());

		Log.i("SUBSCR INNER", String.format("Triggering update."));
		triggerUpdate(null, TimestampBound.notOlderThan(1000 * 60 * 60 * 24)); // Max age, 24 hours
	}

	public void addListener(SubredditSubscriptionStateChangeListener listener) {
		listeners.add(listener);
		Log.i("SUBSCR INNER", "Added listener");
	}

	public synchronized boolean areSubscriptionsReady() {
		return subscriptions != null;
	}

	public synchronized SubscriptionState getSubscriptionState(final String subredditCanonicalId) {

		if(pendingSubscriptions.contains(subredditCanonicalId)) return SubscriptionState.SUBSCRIBING;
		else if(pendingUnsubscriptions.contains(subredditCanonicalId)) return SubscriptionState.UNSUBSCRIBING;
		else if(subscriptions.toHashset().contains(subredditCanonicalId)) return SubscriptionState.SUBSCRIBED;
		else return SubscriptionState.NOT_SUBSCRIBED;
	}

	private synchronized void onSubscriptionAttempt(final String subredditCanonicalId) {
		pendingSubscriptions.add(subredditCanonicalId);
		listeners.map(notifier, SubredditSubscriptionChangeType.SUBSCRIPTION_ATTEMPTED);
	}

	private synchronized void onUnsubscriptionAttempt(final String subredditCanonicalId) {
		pendingUnsubscriptions.add(subredditCanonicalId);
		listeners.map(notifier, SubredditSubscriptionChangeType.UNSUBSCRIPTION_ATTEMPTED);
	}

	private synchronized void onNewSubscriptionListReceived(HashSet<String> newSubscriptions, long timestamp) {

		pendingSubscriptions.clear();
		pendingUnsubscriptions.clear();

		subscriptions = new WritableHashSet(newSubscriptions, timestamp, user.getCanonicalUsername());

		// TODO threaded? or already threaded due to cache manager
		db.put(subscriptions);

		listeners.map(notifier, SubredditSubscriptionChangeType.LIST_UPDATED);
	}

	public synchronized ArrayList<String> getSubscriptionList() {
		return new ArrayList<String>(subscriptions.toHashset());
	}

	public void triggerUpdate(final RequestResponseHandler<HashSet<String>, SubredditRequestFailure> handler, TimestampBound timestampBound) {

		if(subscriptions != null && timestampBound.verifyTimestamp(subscriptions.getTimestamp())) {
			Log.i("SUBSCR INNER", "update not needed");
			return;
		}

		if(subscriptions != null) {
			Log.i("SUBSCR INNER EXISTING TIME", String.format("%d", subscriptions.getTimestamp()));
		}

		Log.i("SUBSCR INNER", String.format("triggerUpdate."));

		new RedditAPIIndividualSubredditListRequester(context, user).performRequest(
				RedditSubredditManager.SubredditListType.SUBSCRIBED,
				timestampBound,
				new RequestResponseHandler<WritableHashSet, SubredditRequestFailure>() {

					// TODO handle failed requests properly -- retry? then notify listeners
					@Override
					public void onRequestFailed(SubredditRequestFailure failureReason) {
						failureReason.t.printStackTrace();
						if(handler != null) handler.onRequestFailed(failureReason);
					}

					@Override
					public void onRequestSuccess(WritableHashSet result, long timeCached) {
						final HashSet<String> newSubscriptions = result.toHashset();
						onNewSubscriptionListReceived(newSubscriptions, timeCached);
						if(handler != null) handler.onRequestSuccess(newSubscriptions, timeCached);
					}
				}
		);

	}

	public void subscribe(final String subredditCanonicalId) {

		// TODO send request
		// TODO trigger update

		onSubscriptionAttempt(subredditCanonicalId);
	}

	public void unsubscribe(final String subredditCanonicalId) {

		// TODO send request
		// TODO trigger update

		onUnsubscriptionAttempt(subredditCanonicalId);
	}

	public Long getSubscriptionListTimestamp() {
		return subscriptions != null ? subscriptions.getTimestamp() : null;
	}

	public interface SubredditSubscriptionStateChangeListener {
		public void onSubredditSubscriptionListUpdated(RedditSubredditSubscriptionManager subredditSubscriptionManager);
		public void onSubredditSubscriptionAttempted(RedditSubredditSubscriptionManager subredditSubscriptionManager);
		public void onSubredditUnsubscriptionAttempted(RedditSubredditSubscriptionManager subredditSubscriptionManager);
	}

	private static enum SubredditSubscriptionChangeType {LIST_UPDATED, SUBSCRIPTION_ATTEMPTED, UNSUBSCRIPTION_ATTEMPTED}

	private class SubredditSubscriptionStateChangeNotifier
			implements WeakReferenceListManager.ArgOperator<SubredditSubscriptionStateChangeListener, SubredditSubscriptionChangeType> {

		public void operate(SubredditSubscriptionStateChangeListener listener, SubredditSubscriptionChangeType changeType) {
			Log.i("SUBSCR SubredditSubscriptionStateChangeNotifier", changeType.toString());
			switch(changeType) {
				case LIST_UPDATED:
					listener.onSubredditSubscriptionListUpdated(RedditSubredditSubscriptionManager.this);
					break;
				case SUBSCRIPTION_ATTEMPTED:
					listener.onSubredditSubscriptionAttempted(RedditSubredditSubscriptionManager.this);
					break;
				case UNSUBSCRIPTION_ATTEMPTED:
					listener.onSubredditUnsubscriptionAttempted(RedditSubredditSubscriptionManager.this);
					break;
				default:
					throw new UnexpectedInternalStateException("Invalid SubredditSubscriptionChangeType " + changeType.toString());
			}
		}
	}
}
