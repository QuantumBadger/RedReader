package org.quantumbadger.redreader.receivers;

import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.TaskStackBuilder;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Handler;
import android.os.Looper;
import android.support.v4.app.NotificationCompat;
import org.apache.http.StatusLine;
import org.holoeverywhere.preference.PreferenceManager;
import org.holoeverywhere.preference.SharedPreferences;
import org.holoeverywhere.widget.TextView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.activities.MainActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.*;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedArray;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedComment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedMessage;
import org.quantumbadger.redreader.reddit.things.RedditComment;
import org.quantumbadger.redreader.reddit.things.RedditMessage;
import org.quantumbadger.redreader.reddit.things.RedditThing;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

import java.net.URI;
import java.util.UUID;

/**
 * Created by Dylan on 10/30/2014.
 */

public class NewMessageChecker extends BroadcastReceiver {

    private CacheRequest request;
    private final String PREFS_FILENAME = "NewMessageChecker";
    private final String PREFS_SAVED_MESSAGE = "LastMessage";

    public void onReceive(Context context, Intent intent) {

        boolean notificationsEnabled = PrefsUtility.pref_behaviour_notifications(context, PreferenceManager.getDefaultSharedPreferences(context));
        if (!notificationsEnabled) return;

        final RedditAccount user = RedditAccountManager.getInstance(context).getDefaultAccount();
        final CacheManager cm = CacheManager.getInstance(context);

        final URI url;

        url = Constants.Reddit.getUri("/message/unread.json?mark=true&limit=2");

        request = new CacheRequest(url, user, null, Constants.Priority.API_INBOX_LIST, 0, CacheRequest.DownloadType.FORCE, Constants.FileType.INBOX_LIST, true, true, true, context) {

            @Override
            protected void onDownloadNecessary() {}

            @Override protected void onDownloadStarted() {}

            @Override
            protected void onCallbackException(final Throwable t) {
                request = null;
                BugReportActivity.handleGlobalError(context, t);
            }

            @Override protected void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {}

            @Override protected void onProgress(final long bytesRead, final long totalBytes) {}

            @Override
            protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {
                request = null;
            }

            @Override
            public void onJsonParseStarted(final JsonValue value, final long timestamp, final UUID session, final boolean fromCache) {

                try {

                    final JsonBufferedObject root = value.asObject();
                    final JsonBufferedObject data = root.getObject("data");
                    final JsonBufferedArray children = data.getArray("children");

                    try {
                        JsonValue newMessage = children.get(0);
                        int numMessages = children.getCurrentItemCount();

                        android.content.SharedPreferences messageStore = context.getSharedPreferences(PREFS_FILENAME, 0);
                        String oldMessage = messageStore.getString(PREFS_SAVED_MESSAGE, "No new messages");
                        if (newMessage.toString().equals(oldMessage)) return;
                        messageStore.edit().putString(PREFS_SAVED_MESSAGE, newMessage.toString()).commit();

                        RedditThing thing = newMessage.asObject(RedditThing.class);
                        String title;
                        String text;
                        Class theClass;
                        if (numMessages > 1) {
                            title = "New messages on Reddit";
                            text = "Touch to view";
                            theClass = MainActivity.class;
                        } else {
                            switch (thing.getKind()) {
                                case COMMENT:
                                    final RedditComment comment = thing.asComment();
                                    title = comment.author + " replied to your comment";
                                    text = "Touch to view";
                                    theClass = MainActivity.class;
                                    break;

                                case MESSAGE:
                                    final RedditMessage message = thing.asMessage();
                                    title = message.author + " sent you a message";
                                    text = "Touch to view";
                                    theClass = MainActivity.class;
                                    break;

                                default:
                                    throw new RuntimeException("Unknown item in list.");
                            }
                        }
                        createNotification(title, text, context, theClass);
                    } catch (IndexOutOfBoundsException e) {} // No new messages

                } catch (Throwable t) {
                    notifyFailure(RequestFailureType.PARSE, t, null, "Parse failure");
                }
            }
        };

        cm.makeRequest(request);

    }

    private void createNotification(String title, String text, Context context, Class theClass) {

        NotificationCompat.Builder theNotification = new NotificationCompat.Builder(context)
                .setSmallIcon(R.drawable.icon)
                .setContentTitle(title)
                .setContentText(text)
                .setAutoCancel(true);

        Intent resultIntent = new Intent(context, theClass);
        resultIntent.putExtra("isNewMessage", true);

        TaskStackBuilder stackBuilder = TaskStackBuilder.create(context);
        stackBuilder.addParentStack(theClass);
        stackBuilder.addNextIntent(resultIntent);
        PendingIntent resultPendingIntent = stackBuilder.getPendingIntent(0, PendingIntent.FLAG_UPDATE_CURRENT);
        theNotification.setContentIntent(resultPendingIntent);

        NotificationManager nm = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);

        nm.notify(0, theNotification.getNotification());
    }
}
