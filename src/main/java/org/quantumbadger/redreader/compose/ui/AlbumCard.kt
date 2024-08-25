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
import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.systemBars
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.draw.shadow
import androidx.compose.ui.graphics.RectangleShape
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.semantics.Role
import androidx.compose.ui.semantics.contentDescription
import androidx.compose.ui.semantics.role
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.common.UriString
import org.quantumbadger.redreader.compose.ctx.RRComposeContextTest
import org.quantumbadger.redreader.compose.prefs.LocalComposePrefs
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme
import org.quantumbadger.redreader.compose.theme.StyledText
import org.quantumbadger.redreader.compose.theme.combinedClickableWithHaptics
import org.quantumbadger.redreader.image.ImageInfo
import org.quantumbadger.redreader.image.ImageSize
import org.quantumbadger.redreader.image.ImageUrlInfo

@Composable
fun AlbumCard(
	index: Int,
	image: ImageInfo,
	onClick: (Int) -> Unit,
	onLongClick: (Int) -> Unit,
) {
	val prefs = LocalComposePrefs.current
	val theme = LocalComposeTheme.current

	val preview = if (image.mediaType == ImageInfo.MediaType.IMAGE) {
		image.preview ?: image.original
	} else {
		image.preview ?: image.bigSquare
	}

	val systemBarsHeight = WindowInsets.systemBars.let { insets ->
		with(LocalDensity.current) {
			(insets.getTop(this) + insets.getBottom(this)).toDp()
		}
	}

	val usableHeight = LocalConfiguration.current.screenHeightDp.dp - systemBarsHeight
	val maxImageHeight = (usableHeight * 6) / 7

	val horizontalPadding = if (prefs.albumGridHorizontalPadding.value) {
		12.dp
	} else {
		0.dp
	}

	Box(
		modifier = Modifier
			.fillMaxWidth()
			.padding(horizontal = horizontalPadding, vertical = 6.dp)
	) {
		val description = stringResource(R.string.album_image_default_text, index + 1)

		val shape = if (prefs.albumGridRoundedCorners.value) {
			RoundedCornerShape(6.dp)
		} else {
			RectangleShape
		}

		Box(
			modifier = Modifier
				.fillMaxWidth()
				.semantics {
					role = Role.Button
					contentDescription = description
				}
				.shadow(3.dp, shape)
				.clip(shape)
				.background(theme.postCard.backgroundColor)
				.combinedClickableWithHaptics(
					onClick = { onClick(index) },
					onLongClick = { onLongClick(index) },
				)
		) {
			Column(Modifier.fillMaxWidth()) {
				if (preview != null) {
					NetImage(
						modifier = Modifier
							.fillMaxWidth()
							.heightIn(max = maxImageHeight),
						image = preview,
						showVideoPlayOverlay = (image.mediaType == ImageInfo.MediaType.VIDEO
								|| image.mediaType == ImageInfo.MediaType.GIF
								|| image.isAnimated == true),
					)
				}

				val title = image.title?.trim()?.takeUnless { it.isEmpty() }
					?: description.takeIf { preview == null }
				val caption = image.caption?.trim()?.takeUnless { it.isEmpty() }

				if (title != null && caption != null) {
					Column(
						modifier = Modifier.padding(14.dp)
					) {
						theme.postCard.title.StyledText(title)
						theme.postCard.subtitle.StyledText(caption)
					}
				} else {
					val text = title ?: caption

					if (text != null) {
						Column(
							modifier = Modifier.padding(14.dp)
						) {
							theme.postCard.caption.StyledText(text)
						}
					}
				}

				if (image.outboundUrl != null) {
					Box(Modifier.padding(12.dp)) {
						RRLinkButton(
							title = stringResource(R.string.album_link_button),
							link = image.outboundUrl,
							theme = theme.linkButton
						)
					}
				}

				AnimatedVisibility(visible = prefs.albumCardShowButtons.value) {
					AlbumEntryButtons(Modifier.fillMaxWidth(), image.original)
				}
			}
		}
	}
}


@Composable
@Preview(backgroundColor = 0x999999)
fun PreviewAlbumCard() {
	RRComposeContextTest {
		AlbumCard(
			0,
			ImageInfo(
				original = ImageUrlInfo(UriString("testimage"), size = ImageSize(100, 100)),
				title = "Test title",
				caption = "Test caption",
				hasAudio = ImageInfo.HasAudio.NO_AUDIO,
			),
			{},
			{}
		)
	}
}
