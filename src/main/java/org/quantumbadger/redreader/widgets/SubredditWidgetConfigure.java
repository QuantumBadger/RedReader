package org.quantumbadger.redreader.widgets;

import android.appwidget.AppWidgetManager;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.view.View;

import org.holoeverywhere.ArrayAdapter;
import org.holoeverywhere.app.Activity;
import org.holoeverywhere.widget.Spinner;
import org.quantumbadger.redreader.R;

/**
 * Created by Andrew on 8/22/13.
 */
public class SubredditWidgetConfigure extends Activity {
    static final String TAG = "SubredditWidgetConfigure";

    private static final String PREFS_NAME
            = "org.quantumbadger.widgets.SubredditWidgetProvider";
    private static final String PREF_PREFIX_KEY = "prefix_";
    public static final String[] SUBREDDITS = {"adviceanimals", "askreddit", "askscience", "aww", "bestof", "books"};
    private Spinner mSubredditSpinner;
    private int mAppWidgetId = AppWidgetManager.INVALID_APPWIDGET_ID;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        // Set the result to CANCELED.  This will cause the widget host to cancel
        // out of the widget placement if they press the back button.
        setResult(RESULT_CANCELED);

        setContentView(R.layout.widget_subreddit_config);

        mSubredditSpinner = (Spinner) findViewById(R.id.subreddit_spinner);
        ArrayAdapter<String> arrayAdapter = new ArrayAdapter<String>(this,
                android.R.layout.simple_spinner_item, SUBREDDITS);
        mSubredditSpinner.setAdapter(arrayAdapter);

        // Bind the action for the save button.
        findViewById(R.id.save_button).setOnClickListener(mOnClickListener);

        // Find the widget id from the intent.
        Intent intent = getIntent();
        Bundle extras = intent.getExtras();
        if (extras != null) {
            mAppWidgetId = extras.getInt(
                    AppWidgetManager.EXTRA_APPWIDGET_ID, AppWidgetManager.INVALID_APPWIDGET_ID);
        }

        // If they gave us an intent without the widget id, just bail.
        if (mAppWidgetId == AppWidgetManager.INVALID_APPWIDGET_ID) {
            finish();
        }
    }

    View.OnClickListener mOnClickListener = new View.OnClickListener() {
        public void onClick(View v) {
            final Context context = SubredditWidgetConfigure.this;

            // When the button is clicked, save the string in our prefs and return that they
            // clicked OK.
            String subreddit = mSubredditSpinner.getSelectedItem().toString();
            saveSubredditPref(context, mAppWidgetId, subreddit);

            // Push widget update to surface with newly set prefix
            AppWidgetManager appWidgetManager = AppWidgetManager.getInstance(context);
            SubredditWidgetProvider.updateAppWidget(context, appWidgetManager,
                    mAppWidgetId, subreddit);

            // Make sure we pass back the original appWidgetId
            Intent resultValue = new Intent();
            resultValue.putExtra(AppWidgetManager.EXTRA_APPWIDGET_ID, mAppWidgetId);
            setResult(RESULT_OK, resultValue);
            finish();
        }
    };

    static void saveSubredditPref(Context context, int mAppWidgetId, String subreddit) {
        SharedPreferences.Editor prefs = context.getSharedPreferences(PREFS_NAME, 0).edit();
        prefs.putString(PREF_PREFIX_KEY + mAppWidgetId, subreddit);
        prefs.commit();
    }

    static String loadSubredditPref(Context context, int appWidgetId) {
        SharedPreferences prefs = context.getSharedPreferences(PREFS_NAME, 0);
        String prefix = prefs.getString(PREF_PREFIX_KEY + appWidgetId, null);
        if (prefix != null) {
            return prefix;
        } else {
            return "all";//TODO Default value
        }
    }
}
