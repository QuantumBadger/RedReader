package org.quantumbadger.redreader.widgets;


import android.app.PendingIntent;
import android.appwidget.AppWidgetManager;
import android.appwidget.AppWidgetProvider;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.widget.RemoteViews;
import android.widget.Toast;

import org.quantumbadger.redreader.R;

import java.util.Random;

public class SubredditWidgetProvider extends AppWidgetProvider {


    private static final String ACTION_REFRESH = "org.quantumbadger.redreader.widgets.REFRESH";


    @Override
    public void onReceive(Context context, Intent intent) {
        final String action = intent.getAction();
        // determine added new task.
        if (action.equals(ACTION_REFRESH)) {
            Toast.makeText(context, "REFRESH",
                    Toast.LENGTH_SHORT).show();
        }
        // notify the widget that an action has appeared.
/*        final AppWidgetManager mgr = AppWidgetManager.getInstance(context);
        final ComponentName cn = new ComponentName(context, ListWidget.class);
        mgr.notifyAppWidgetViewDataChanged(mgr.getAppWidgetIds(cn),
                R.id.widget_list);*/
        super.onReceive(context, intent);
    }

    @Override
    public void onUpdate(Context context, AppWidgetManager appWidgetManager,
                         int[] appWidgetIds) {

        // Get all ids
        ComponentName thisWidget = new ComponentName(context,
                SubredditWidgetProvider.class);
        int[] allWidgetIds = appWidgetManager.getAppWidgetIds(thisWidget);

        for (int widgetId : allWidgetIds) {
            // Create some random data
            int number = (new Random().nextInt(100));

            RemoteViews remoteViews = new RemoteViews(context.getPackageName(),
                    R.layout.widget_subreddit);

            // Set the text to the view with the id R.id.update
            // instead of -1
            remoteViews.setTextViewText(R.id.update_tv, String.valueOf(number));

            // Register an onClickListener
            Intent intent = new Intent(context, SubredditWidgetProvider.class);

            intent.setAction(AppWidgetManager.ACTION_APPWIDGET_UPDATE);
            intent.putExtra(AppWidgetManager.EXTRA_APPWIDGET_IDS, allWidgetIds);

            PendingIntent pendingIntent = PendingIntent.getBroadcast(context,
                    0, intent, PendingIntent.FLAG_UPDATE_CURRENT);
            remoteViews.setOnClickPendingIntent(R.id.refresh_btn, pendingIntent);
            appWidgetManager.updateAppWidget(widgetId, remoteViews);
        }
    }
}
