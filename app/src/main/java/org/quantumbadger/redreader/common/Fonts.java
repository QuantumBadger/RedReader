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

package org.quantumbadger.redreader.common;

import android.content.res.AssetManager;
import android.graphics.Typeface;
import android.util.Log;
import androidx.annotation.NonNull;

import java.util.concurrent.atomic.AtomicReference;

public final class Fonts {

	private static final String TAG = "Fonts";

	@NonNull private static final AtomicReference<Typeface> sVeraMono = new AtomicReference<>();
	@NonNull private static final AtomicReference<Typeface> sRobotoLight = new AtomicReference<>();

	private Fonts() {}

	public static void onAppCreate(@NonNull final AssetManager assetManager) {

		General.startNewThread("FontCreate", () -> {

			try {
				sVeraMono.set(Typeface.createFromAsset(assetManager, "fonts/VeraMono.ttf"));
				sRobotoLight.set(Typeface.createFromAsset(assetManager, "fonts/Roboto-Light.ttf"));

				Log.i(TAG, "Fonts created");

			} catch(final Exception e) {
				Log.e(TAG, "Got exception while creating fonts", e);
			}
		});
	}

	@NonNull
	public static Typeface getVeraMonoOrAlternative() {

		final Typeface result = sVeraMono.get();

		if(result == null) {
			return Typeface.MONOSPACE;
		} else {
			return result;
		}
	}

	@NonNull
	public static Typeface getRobotoLightOrAlternative() {

		final Typeface result = sRobotoLight.get();

		if(result == null) {
			return Typeface.DEFAULT;
		} else {
			return result;
		}
	}
}
