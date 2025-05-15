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

package org.quantumbadger.redreader.compose.ctx

import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import org.quantumbadger.redreader.compose.prefs.ComposePrefs
import org.quantumbadger.redreader.compose.prefs.LocalComposePrefs
import org.quantumbadger.redreader.compose.prefs.Preference
import org.quantumbadger.redreader.compose.theme.RRComposeContextTheme
import org.quantumbadger.redreader.settings.types.AlbumViewMode
import org.quantumbadger.redreader.settings.types.AppearanceTheme

@Composable
fun RRComposeContextTest(content: @Composable () -> Unit) {

	val prefValues = object : ComposePrefs {
		override val appearanceTheme: Preference<AppearanceTheme> = testPref(AppearanceTheme.RED)
		override val appearanceFontScaleGlobal = 1f
		override val appearanceFontScaleBodyText = 1f
		override val appearanceFontScalePosts = 1f
		override val appearanceFontScalePostSubtitles = 1f
		override val albumViewMode = testPref(AlbumViewMode.Cards)
		override val albumCardShowButtons = testPref(true)
		override val albumListShowThumbnails = testPref(true)
		override val albumGridCropToSquare = testPref(false)
		override val albumGridColumns = testPref(3)
		override val albumListThumbnailSize = testPref(64)
		override val albumListShowButtons = testPref(true)
		override val albumGridRoundedCorners = testPref(true)
		override val albumGridHorizontalPadding = testPref(true)
		override val albumCompactTitle = testPref(false)
	}

	CompositionLocalProvider(
		LocalComposePrefs provides prefValues,
		LocalLauncher provides {},
	) {
		RRComposeContextTheme {
			content()
		}
	}
}

private fun <T> testPref(value: T) = object : Preference<T> {
	override var value: T
		get() = value
		set(value) {}
}
