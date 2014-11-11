package org.quantumbadger.redreader.receivers;

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import org.quantumbadger.redreader.activities.MainActivity;

/**
 * Created by Dylan on 11/11/2014.
 */
public class BootCompletedReceiver extends BroadcastReceiver {

    private PendingIntent pendingIntent;
    private AlarmManager alarmManager;

    @Override
    public void onReceive(Context context, Intent intent) {
        if (intent.getAction().equals("android.intent.action.BOOT_COMPLETED")) {
            Intent alarmIntent = new Intent(context, NewMessageChecker.class);
            pendingIntent = PendingIntent.getBroadcast(context, 0, alarmIntent, 0);

            alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
            alarmManager.setRepeating(AlarmManager.RTC_WAKEUP, System.currentTimeMillis(), 20000, pendingIntent); //TODO make length between checks a setting
        }
    }
}
