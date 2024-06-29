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
package org.quantumbadger.redreader.compose.ui

import androidx.compose.animation.AnimatedVisibility
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.constraintlayout.compose.ConstraintLayout
import androidx.constraintlayout.compose.Dimension
import org.quantumbadger.redreader.compose.ctx.RRComposeContextTest
import org.quantumbadger.redreader.compose.prefs.LocalComposePrefs
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme
import org.quantumbadger.redreader.image.ImageInfo
import org.quantumbadger.redreader.image.ImageSize
import org.quantumbadger.redreader.image.ImageUrlInfo

@Composable
fun AlbumListItem(
	index: Int,
	image: ImageInfo
) {
	val prefs = LocalComposePrefs.current
	val theme = LocalComposeTheme.current

	val preview = image.bigSquare ?: image.preview

	ConstraintLayout(
		modifier = Modifier
			.fillMaxWidth()
			.background(theme.postCard.backgroundColor),
	) {
		val (thumbnail, text, buttons) = createRefs()

		Box(
            Modifier
                .background(theme.postCard.previewImageBackgroundColor)
                .constrainAs(thumbnail) {
                    top.linkTo(parent.top)
                    bottom.linkTo(parent.bottom)
                    start.linkTo(parent.start)
                }) {
			if (prefs.albumListShowThumbnails.value) {
				NetImage(
					modifier = Modifier.width(prefs.albumListThumbnailSize.value.dp),
					image = preview,
					cropToAspect = 1f,
				)
			}
		}

		val title = image.title?.trim()?.takeUnless { it.isEmpty() }
		val caption = image.caption?.trim()?.takeUnless { it.isEmpty() }

		Column(
			modifier = Modifier.padding(14.dp).constrainAs(text) {
				top.linkTo(parent.top)
				bottom.linkTo(parent.bottom)
				start.linkTo(thumbnail.end)
				end.linkTo(buttons.start)
				width = Dimension.fillToConstraints
			}
		) {
			if (title != null && caption != null) {
				Text(
					text = title,
					style = theme.postCard.title,
				)
				Spacer(Modifier.height(4.dp))
				Text(
					text = caption,
					style = theme.postCard.subtitle,
				)

			} else {
				Text(
					text = title ?: caption ?: "Image ${index + 1}",
					style = theme.postCard.caption,
				)
			}
		}

		// TODO separate pref for list buttons
		// TODO buttons in separate composable
		AnimatedVisibility(
			modifier = Modifier.constrainAs(buttons) {
				top.linkTo(parent.top)
				bottom.linkTo(parent.bottom)
				end.linkTo(parent.end)
			},
			visible = prefs.albumListShowButtons.value
		) {
			AlbumEntryButtons()
		}
	}
}


@Composable
@Preview(backgroundColor = 0x999999)
fun PreviewAlbumListItem() {
	RRComposeContextTest {
		AlbumListItem(
			2,
			ImageInfo(
				original = ImageUrlInfo("testimage", size = ImageSize(100, 100)),
				title = "Test title which is very long",
				caption = null,
				hasAudio = ImageInfo.HasAudio.NO_AUDIO,
			)
		)
	}
}
