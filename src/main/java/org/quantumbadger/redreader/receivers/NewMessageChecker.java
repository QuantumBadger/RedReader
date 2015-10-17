/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package com.konneh.scroll.receivers;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import android.util.Log;
import org.apache.http.StatusLine;
import com.konneh.scroll.R;
import com.konneh.scroll.account.RedditAccount;
import com.konneh.scroll.account.RedditAccountManager;
import com.konneh.scroll.activities.BugReportActivity;
import com.konneh.scroll.activities.InboxListingActivity;
import com.konneh.scroll.cache.CacheManager;
import com.konneh.scroll.cache.CacheRequest;
import com.konneh.scroll.cache.RequestFailureType;
import com.konneh.scroll.common.Constants;
import com.konneh.scroll.common.PrefsUtility;
import com.konneh.scroll.jsonwrap.JsonBufferedArray;
import com.konneh.scroll.jsonwrap.JsonBufferedObject;
import com.konneh.scroll.jsonwrap.JsonValue;
import com.konneh.scroll.reddit.things.RedditComment;
import com.konneh.scroll.reddit.things.RedditMessage;
import com.konneh.scroll.reddit.things.RedditThing;

import java.net.URI;
import java.util.UUID;

public class NewMessageChecker extends BroadcastReceiver {

	private static final String PREFS_SAVED_MESSAGE_ID = "LastMessageId";
	private static final String PREFS_SAVED_MESSAGE_TIMESTAMP = "LastMessageTimestamp";


	public void onReceive(Context context, Intent intent) {
		checkForNewMessages(context);
	}

	public static void checkForNewMessages(Context context) {

		Log.i("Scroll", "Checking for new messages.");

		boolean notificationsEnabled = PrefsUtility.pref_behaviour_notifications(context, PreferenceManager.getDefaultSharedPreferences(context));
		if (!notificationsEnabled) return;

		final RedditAccount user = RedditAccountManager.getInstance(context).getDefaultAccount();

		if(user.isAnonymous()) {
			return;
		}

		final CacheManager cm = CacheManager.getInstance(context);

		final URI url = Constants.Reddit.getUri("/message/unread.json?limit=2");

		final CacheRequest request = new CacheRequest(
				url,
				user,
				null,
				Constants.Priority.API_INBOX_LIST,
				0,
				CacheRequest.DownloadType.FORCE,
				Constants.FileType.INBOX_LIST,
				true,
				true,
				true,
				context) {

			@Override
			protected void onDownloadNecessary() {}

			@Override
			protected void onDownloadStarted() {}

			@Override
			protected void onCallbackException(final Throwable t) {
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {
				Log.e("NewMessageChecker", "Request failed", t);
			}

			@Override
			protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {}

			@Override
			protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {}

			@Override
			public void onJsonParseStarted(final JsonValue value, final long timestamp, final UUID session, final boolean fromCache) {

				try {

					final JsonBufferedObject root = value.asObject();
					final JsonBufferedObject data = root.getObject("data");
					final JsonBufferedArray children = data.getArray("children");

					children.join();
					final int messageCount = children.getCurrentItemCount();

					if(messageCount < 1) {
						return;
					}

					final RedditThing thing = children.get(0).asObject(RedditThing.class);

					String title;
					final String text = context.getString(R.string.notification_message_action);

					final String messageID;
					final long messageTimestamp;

					switch(thing.getKind()) {
						case COMMENT: {
							final RedditComment comment = thing.asComment();
							title = comment.author + " " + context.getString(R.string.notification_comment);
							messageID = comment.name;
							messageTimestamp = comment.created_utc;
							break;
						}

						case MESSAGE: {
							final RedditMessage message = thing.asMessage();
							title = message.author + " " + context.getString(R.string.notification_message);
							messageID = message.name;
							messageTimestamp = message.created_utc;
							break;
						}

						default: {
							throw new RuntimeException("Unknown item in list.");
						}
					}

					// Check if the previously saved message is the same as the one we just received

					final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(context);
					final String oldMessageId = prefs.getString(PREFS_SAVED_MESSAGE_ID, "");
					final long oldMessageTimestamp = prefs.getLong(PREFS_SAVED_MESSAGE_TIMESTAMP, 0);

					if(oldMessageId == null || (!messageID.equals(oldMessageId) && oldMessageTimestamp <= messageTimestamp)) {

						prefs.edit()
								.putString(PREFS_SAVED_MESSAGE_ID, messageID)
								.putLong(PREFS_SAVED_MESSAGE_TIMESTAMP, messageTimestamp)
								.commit();

						if(messageCount > 1) {
							title = context.getString(R.string.notification_message_multiple);
						}

						createNotification(title, text, context);
					}

				} catch(Throwable t) {
					notifyFailure(RequestFailureType.PARSE, t, null, "Parse failure");
				}
			}
		};

		cm.makeRequest(request);
	}

	private static void createNotification(String title, String text, Context context) {

		final Notification.Builder notification = new Notification.Builder(context)
				.setSmallIcon(R.drawable.icon_inv)
				.setContentTitle(title)
				.setContentText(text)
				.setAutoCancel(true);

		final Intent intent = new Intent(context, InboxListingActivity.class);
		notification.setContentIntent(PendingIntent.getActivity(context, 0, intent, 0));

		final NotificationManager nm = (NotificationManager)context.getSystemService(Context.NOTIFICATION_SERVICE);
		nm.notify(0, notification.getNotification());
	}
}
