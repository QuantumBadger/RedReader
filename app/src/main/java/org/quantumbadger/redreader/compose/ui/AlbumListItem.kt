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
import androidx.compose.animation.slideInHorizontally
import androidx.compose.animation.slideOutHorizontally
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.constraintlayout.compose.ConstraintLayout
import androidx.constraintlayout.compose.Dimension
import androidx.constraintlayout.compose.atLeast
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.common.UriString
import org.quantumbadger.redreader.compose.ctx.Dest
import org.quantumbadger.redreader.compose.ctx.LocalLauncher
import org.quantumbadger.redreader.compose.ctx.RRComposeContextTest
import org.quantumbadger.redreader.compose.prefs.LocalComposePrefs
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme
import org.quantumbadger.redreader.compose.theme.StyledText
import org.quantumbadger.redreader.compose.theme.combinedClickableWithHaptics
import org.quantumbadger.redreader.image.ImageInfo
import org.quantumbadger.redreader.image.ImageSize
import org.quantumbadger.redreader.image.ImageUrlInfo
import java.util.Locale

@Composable
fun AlbumListItem(
	index: Int,
	image: ImageInfo,
	onClick: (Int) -> Unit,
	onLongClick: (Int) -> Unit,
) {
	val prefs = LocalComposePrefs.current
	val theme = LocalComposeTheme.current

	val thumbnailImage = image.bigSquare ?: image.preview

	val thumbnailSize = prefs.albumListThumbnailSize.value.dp

	ConstraintLayout(
		modifier = Modifier
			.fillMaxWidth()
			.background(theme.postCard.backgroundColor)
			.combinedClickableWithHaptics(
				onClick = { onClick(index) },
				onLongClick = { onLongClick(index) }
			),
	) {
		val (thumbnail, text, buttons) = createRefs()

		Box(
			Modifier
				.constrainAs(thumbnail) {
					top.linkTo(parent.top)
					bottom.linkTo(parent.bottom)
					start.linkTo(parent.start)
					height = Dimension.fillToConstraints.atLeast(thumbnailSize)
				}
				.background(theme.postCard.previewImageBackgroundColor),
			contentAlignment = Alignment.Center
		) {
			if (prefs.albumListShowThumbnails.value && thumbnailImage != null) {
				NetImage(
					modifier = Modifier.width(thumbnailSize),
					image = thumbnailImage,
					cropToAspect = 1f,
				)
			}
		}

		val title = remember(image) {
			image.title?.trim()?.takeUnless { it.isEmpty() }
		}

		val caption = remember(image) {
			image.caption?.trim()?.takeUnless { it.isEmpty() } ?: listOfNotNull(
				image.type?.run {
					when (this) {
						"image/png" -> "PNG"
						"image/gif" -> "GIF"
						"image/jpg", "image/jpeg" -> "JPEG"
						else -> this
					}
				},
				image.original.size?.run { "${width}x$height" },
				image.original.sizeBytes?.let {
					if (it < 512 * 1024) {
						String.format(Locale.US, "%.1f kB", it.toFloat() / 1024)
					} else {
						String.format(
							Locale.US,
							"%.1f MB",
							it.toFloat() / (1024 * 1024)
						)
					}
				}
			)
				.joinToString(separator = ", ")
				.takeUnless { it.isEmpty() }
		}

		Column(
			modifier = Modifier
				.padding(vertical = 8.dp, horizontal = 12.dp)
				.constrainAs(text) {
					top.linkTo(parent.top)
					bottom.linkTo(parent.bottom)
					start.linkTo(thumbnail.end)
					end.linkTo(buttons.start)
					width = Dimension.fillToConstraints
				}
		) {
			theme.postCard.title.StyledText(title ?: stringResource(R.string.album_image_default_text, index + 1))

			if (caption != null) {
				Spacer(Modifier.height(4.dp))
				theme.postCard.subtitle.StyledText(caption)
			}

			if (image.outboundUrl != null) {
				Spacer(Modifier.height(12.dp))

				RRLinkButton(
					title = stringResource(R.string.album_link_button),
					link = image.outboundUrl,
					theme = theme.linkButton
				)
			}
		}

		Box(
			modifier = Modifier.constrainAs(buttons) {
				top.linkTo(parent.top)
				bottom.linkTo(parent.bottom)
				end.linkTo(parent.end)
			}
		) {
			AnimatedVisibility(
				visible = prefs.albumListShowButtons.value,
				enter = slideInHorizontally { it },
				exit = slideOutHorizontally { it }
			) {
				val launch = LocalLauncher.current

				RRIconButton(
					onClick = { launch(Dest.LinkLongClick(image.original.url)) },
					icon = R.drawable.dots_vertical_dark,
					contentDescription = R.string.three_dots_menu
				)
			}
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
				original = ImageUrlInfo(UriString("testimage"), size = ImageSize(100, 100)),
				title = "Test title which is very long",
				caption = null,
				hasAudio = ImageInfo.HasAudio.NO_AUDIO,
				outboundUrl = UriString("https://redreader.org")
			),
			{},
			{}
		)
	}
}
