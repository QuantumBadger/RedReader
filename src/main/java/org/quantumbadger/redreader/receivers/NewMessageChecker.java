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

package org.quantumbadger.redreader.receivers;

import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.database.sqlite.SQLiteDatabaseCorruptException;
import android.graphics.Color;
import android.os.Build;
import android.util.Log;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.InboxListingActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.CacheRequestCallbacks;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyAlways;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.GenericFactory;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.SharedPrefsWrapper;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.receivers.announcements.AnnouncementDownloader;
import org.quantumbadger.redreader.reddit.kthings.JsonUtils;
import org.quantumbadger.redreader.reddit.kthings.RedditComment;
import org.quantumbadger.redreader.reddit.kthings.RedditIdAndType;
import org.quantumbadger.redreader.reddit.kthings.RedditListing;
import org.quantumbadger.redreader.reddit.kthings.RedditMessage;
import org.quantumbadger.redreader.reddit.kthings.RedditThing;
import org.quantumbadger.redreader.reddit.kthings.UrlEncodedString;

import java.io.IOException;
import java.net.URI;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;

public class NewMessageChecker extends BroadcastReceiver {

	private static final String TAG = "NewMessageChecker";

	private static final String NOTIFICATION_CHANNEL_ID = "RRNewMessageChecker";

	public static final String PREFS_SAVED_MESSAGE_ID = "LastMessageId";
	public static final String PREFS_SAVED_MESSAGE_TIMESTAMP = "LastMessageTimestamp";


	@Override
	public void onReceive(final Context context, final Intent intent) {
		checkForNewMessages(context);
		AnnouncementDownloader.performDownload(context);
	}

	public static void checkForNewMessages(final Context context) {

		Log.i("RedReader", "Checking for new messages.");

		final boolean notificationsEnabled = PrefsUtility.pref_behaviour_notifications();
		if(!notificationsEnabled) {
			return;
		}

		final RedditAccount user;

		try {
			user = RedditAccountManager.getInstance(context).getDefaultAccount();

		} catch(final SQLiteDatabaseCorruptException e) {
			// Avoid background crash
			Log.e(TAG, "Accounts database corrupt", e);
			return;
		}

		if(user.isAnonymous()) {
			return;
		}

		final CacheManager cm = CacheManager.getInstance(context);

		final URI url = Constants.Reddit.getUri("/message/unread.json?limit=2");

		final CacheRequest request = new CacheRequest(
				url,
				user,
				null,
				new Priority(Constants.Priority.API_INBOX_LIST),
				DownloadStrategyAlways.INSTANCE,
				Constants.FileType.INBOX_LIST,
				CacheRequest.DOWNLOAD_QUEUE_REDDIT_API,
				false,
				context,
				new CacheRequestCallbacks() {
					@Override
					public void onFailure(@NonNull final RRError error) {

						Log.e(TAG, "Request failed: " + error, error.t);
					}

					@Override
					public void onDataStreamComplete(
							@NonNull final GenericFactory<SeekableInputStream, IOException>
									streamFactory,
							final TimestampUTC timestamp,
							@NonNull final UUID session,
							final boolean fromCache,
							@Nullable final String mimetype) {

						try {
							final RedditThing listingThing = JsonUtils.INSTANCE
									.decodeRedditThingFromStream(streamFactory.create());

							final RedditListing listing
									= ((RedditThing.Listing)listingThing).getData();

							final int messageCount = listing.getChildren().size();

							if(General.isSensitiveDebugLoggingEnabled()) {
								Log.i(TAG, "Got response. Message count = " + messageCount);
							}

							if(messageCount < 1) {
								return;
							}

							final RedditThing thing = listing.getChildren().get(0).ok();

							String title;
							final String text
									= context.getString(R.string.notification_message_action);

							final RedditIdAndType messageID;
							final TimestampUTC messageTimestamp;

							final String unknownUser = "["
									+ context.getString(R.string.general_unknown)
									+ "]";

							if(thing instanceof RedditThing.Comment) {
								final RedditComment comment
										= ((RedditThing.Comment)thing).getData();

								title = context.getString(
										R.string.notification_comment,
										General.nullAlternative(
												General.mapIfNotNull(
														comment.getAuthor(),
														UrlEncodedString::getDecoded),
												unknownUser));

								messageID = comment.getName();
								messageTimestamp = comment.getCreated_utc().getValue();

							} else if(thing instanceof RedditThing.Message) {
								final RedditMessage message
										= ((RedditThing.Message)thing).getData();

								title = context.getString(
										R.string.notification_message,
										General.nullAlternative(
												General.mapIfNotNull(
														message.getAuthor(),
														UrlEncodedString::getDecoded),
												General.mapIfNotNull(
														message.getSubreddit_name_prefixed(),
														UrlEncodedString::getDecoded),
												unknownUser));

								messageID = message.getName();
								messageTimestamp = message.getCreated_utc().getValue();

							} else {
								throw new RuntimeException("Unknown item in list.");
							}

							// Check if the previously saved message is the same as the one we
							// just received

							final SharedPrefsWrapper prefs
									= General.getSharedPrefs(context);
							final String oldMessageId = prefs.getString(
									PREFS_SAVED_MESSAGE_ID,
									"");
							final long oldMessageTimestamp = prefs.getLong(
									PREFS_SAVED_MESSAGE_TIMESTAMP,
									0);

							if(oldMessageId == null || (!messageID.getValue().equals(oldMessageId)
									&& oldMessageTimestamp
											<= messageTimestamp.toUtcSecs())) {

								Log.e(TAG, "New messages detected. Showing notification.");

								prefs.edit()
										.putString(PREFS_SAVED_MESSAGE_ID, messageID.getValue())
										.putLong(
												PREFS_SAVED_MESSAGE_TIMESTAMP,
												messageTimestamp.toUtcSecs())
										.apply();

								if(messageCount > 1) {
									title = context.getString(
											R.string.notification_message_multiple);
								}

								createNotification(title, text, context);

							} else {
								Log.e(TAG, "All messages have been previously seen.");
							}

						} catch(final Exception e) {
							onFailure(General.getGeneralErrorForFailure(
									context,
									CacheRequest.REQUEST_FAILURE_PARSE,
									e,
									null,
									url.toString(),
									FailedRequestBody.from(streamFactory)));
						}
					}
				});

		cm.makeRequest(request);
	}

	private static final AtomicBoolean sChannelCreated = new AtomicBoolean(false);

	private static void createNotification(
			final String title,
			final String text,
			final Context context) {

		final NotificationManager nm = (NotificationManager)context.getSystemService(
				Context.NOTIFICATION_SERVICE);

		synchronized(sChannelCreated) {

			if(!sChannelCreated.getAndSet(true)) {

				if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {

					if(nm.getNotificationChannel(NOTIFICATION_CHANNEL_ID) == null) {

						Log.i(TAG, "Creating notification channel");

						final NotificationChannel channel = new NotificationChannel(
								NOTIFICATION_CHANNEL_ID,
								context.getString(
										R.string.notification_channel_name_reddit_messages),
								NotificationManager.IMPORTANCE_DEFAULT);

						nm.createNotificationChannel(channel);

					} else {
						Log.i(
								TAG,
								"Not creating notification channel as it already exists");
					}

				} else {
					Log.i(
							TAG,
							"Not creating notification channel due to old Android version");
				}
			}
		}

		final NotificationCompat.Builder notification = new NotificationCompat.Builder(
				context)
				.setSmallIcon(R.drawable.icon_notif)
				.setContentTitle(title)
				.setContentText(text)
				.setAutoCancel(true)
				.setChannelId(NOTIFICATION_CHANNEL_ID);

		if(Build.VERSION.SDK_INT >= 21) {
			notification.setColor(Color.rgb(0xd3, 0x2f, 0x2f));
		}

		final Intent intent = new Intent(context, InboxListingActivity.class);

		int flags = 0;

		if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
			flags |= PendingIntent.FLAG_IMMUTABLE;
		}

		notification.setContentIntent(PendingIntent.getActivity(context, 0, intent, flags));

		nm.notify(0, notification.getNotification());
	}
}
