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

package org.quantumbadger.redreader.translation;

import android.content.Context;
import android.os.Handler;
import android.os.Looper;

import androidx.preference.PreferenceManager;

/** Process-wide queue shared by post and comment translation dialogs. */
public final class LocalTranslation {

	private static LocalTranslation instance;

	private final TranslationModelStore models;
	private final TranslationService service;

	private LocalTranslation(final Context context) {
		models = new TranslationModelStore(context);
		final Handler main = new Handler(Looper.getMainLooper());
		service = new TranslationService(new GgufTranslationProvider(models), main::post);
	}

	public static synchronized LocalTranslation getInstance(final Context context) {
		if(instance == null) {
			instance = new LocalTranslation(context.getApplicationContext());
		}
		return instance;
	}

	public TranslationModelStore getModels() {
		return models;
	}

	public TranslationService getService() {
		return service;
	}

	public static String getTargetLanguage(final Context context) {
		return PreferenceManager.getDefaultSharedPreferences(context)
				.getString("translation_target_language", "zh-Hans");
	}
}
