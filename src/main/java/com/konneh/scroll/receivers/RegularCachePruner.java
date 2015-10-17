package com.konneh.scroll.receivers;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;
import com.konneh.scroll.cache.CacheManager;

public class RegularCachePruner extends BroadcastReceiver {

	@Override
	public void onReceive(final Context context, final Intent intent) {

		Log.i("RegularCachePruner", "Pruning cache...");

		new Thread() {
			@Override
			public void run() {
				CacheManager.getInstance(context).pruneCache();
			}
		}.start();
	}
}
