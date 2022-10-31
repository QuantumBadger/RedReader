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

package org.saiditnet.redreader.views;

import android.os.Build;
import android.support.annotation.NonNull;
import android.util.Log;

public abstract class RRChoreographer {

	public interface Callback {
		void doFrame(long frameTimeNanos);
	}

	@NonNull
	public static RRChoreographer getInstance() {

		if(android.os.Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN) {
			Log.i("RRChoreographer", "Using modern Choreographer");
			return RRChoreographerModern.INSTANCE;
		} else {
			Log.i("RRChoreographer", "Using legacy Choreographer");
			return RRChoreographerLegacy.INSTANCE;
		}
	}

	public abstract void postFrameCallback(@NonNull final Callback callback);
}
