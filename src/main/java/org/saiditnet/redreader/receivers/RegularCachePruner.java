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

package org.saiditnet.redreader.receivers;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;
import org.saiditnet.redreader.cache.CacheManager;
import org.saiditnet.redreader.reddit.prepared.RedditChangeDataManager;

public class RegularCachePruner extends BroadcastReceiver {

	@Override
	public void onReceive(final Context context, final Intent intent) {

		Log.i("RegularCachePruner", "Pruning cache...");

		new Thread() {
			@Override
			public void run() {
				RedditChangeDataManager.pruneAllUsers(context);
				CacheManager.getInstance(context).pruneCache();
			}
		}.start();
	}
}
