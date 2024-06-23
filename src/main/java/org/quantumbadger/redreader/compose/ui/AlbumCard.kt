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

import androidx.compose.animation.animateContentSize
import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.foundation.layout.aspectRatio
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.systemBars
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.CircularProgressIndicator
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.draw.shadow
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.account.RedditAccountId
import org.quantumbadger.redreader.common.General
import org.quantumbadger.redreader.compose.ctx.RRComposeContextTest
import org.quantumbadger.redreader.compose.net.NetRequestStatus
import org.quantumbadger.redreader.compose.net.fetchImage
import org.quantumbadger.redreader.compose.prefs.LocalComposePrefs
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme
import org.quantumbadger.redreader.image.ImageInfo
import org.quantumbadger.redreader.image.ImageSize
import org.quantumbadger.redreader.image.ImageUrlInfo

@Composable
fun AlbumCard(
	image: ImageInfo
) {
	val prefs = LocalComposePrefs.current
	val theme = LocalComposeTheme.current

	val preview = image.preview ?: image.original
	val url = General.uriFromString(preview?.url)

	val systemBarsHeight = WindowInsets.systemBars.let { insets ->
		with(LocalDensity.current) {
			(insets.getTop(this) + insets.getBottom(this)).toDp()
		}
	}

	val usableHeight = LocalConfiguration.current.screenHeightDp.dp - systemBarsHeight
	val maxImageHeight = (usableHeight * 6) / 7

	// TODO parse reddit image previews
	// TODO handle videos

	Box(
		modifier = Modifier
            .fillMaxWidth()
            .padding(horizontal = 12.dp, vertical = 6.dp)
	) {
		Column(
			modifier = Modifier
                .shadow(3.dp, RoundedCornerShape(6.dp))
                .clip(RoundedCornerShape(6.dp))
                .background(theme.postCard.backgroundColor)
		) {
			Box(
				modifier = Modifier
                    .fillMaxWidth()
                    .animateContentSize(),
				contentAlignment = Alignment.Center,
			) {
				if (preview != null && url != null) {
					val data by fetchImage(
						uri = url,
						user = RedditAccountId(""), // TODO
					)

					val aspectRatio = preview.size?.takeIf { it.height > 0 }?.let {
						it.width.toFloat() / it.height.toFloat()
					}

					if (data !is NetRequestStatus.Success && aspectRatio != null) {
						Box(
							modifier = Modifier
                                .heightIn(max = maxImageHeight)
                                .fillMaxWidth()
                                .aspectRatio(aspectRatio)
						)
					}

					when (val it = data) {
						NetRequestStatus.Connecting -> {
							CircularProgressIndicator(
								Modifier.padding(24.dp)
							)
						}

						is NetRequestStatus.Downloading -> {
							CircularProgressIndicator(
								modifier = Modifier.padding(24.dp),
								progress = it.fractionComplete
							)
						}

						is NetRequestStatus.Failed -> {
							// TODO
						}

						is NetRequestStatus.Success -> {
							Box(
                                Modifier
                                    .fillMaxWidth()
                                    .background(
                                        theme.postCard.previewImageBackgroundColor
                                    )
							) {
								Image(
									modifier = Modifier
                                        .fillMaxWidth()
                                        .heightIn(max = maxImageHeight),
									bitmap = it.result,
									contentDescription = null
								)
							}
						}
					}

				}
			}


			val title = image.title?.trim()?.takeUnless { it.isEmpty() }
			val caption = image.caption?.trim()?.takeUnless { it.isEmpty() }

			if (title != null && caption != null) {
				Column(
					modifier = Modifier.padding(14.dp)
				) {
					Text(
						text = title,
						style = theme.postCard.title,
					)

					Text(
						text = caption,
						style = theme.postCard.subtitle,
					)
				}
			} else {
				val text = title ?: caption

				if (text != null) {
					Column(
						modifier = Modifier.padding(14.dp)
					) {
						Text(
							text = text,
							style = theme.postCard.caption,
						)
					}
				}
			}

			Row(
				Modifier.fillMaxWidth(),

				// TODO left hand mode
				horizontalArrangement = Arrangement.Absolute.Right
			) {
				RRIconButton(
					icon = R.drawable.ic_action_external_dark,
					contentDescription = R.string.action_external
				) {
					// TODO
				}

				RRIconButton(
					icon = R.drawable.ic_action_save_dark, // TODO download icon
					contentDescription = R.string.action_save_image
				) {
					// TODO
				}

				RRIconButton(
					icon = R.drawable.ic_action_share_dark,
					contentDescription = R.string.action_share_image
				) {
					// TODO
				}

				Spacer(Modifier.width(6.dp))
			}
		}
	}
}


@Composable
@Preview(backgroundColor = 0x999999)
fun PreviewAlbumCard() {
	RRComposeContextTest {
		AlbumCard(
			ImageInfo(
				original = ImageUrlInfo("testimage", size = ImageSize(100, 100)),
				title = "Test title",
				caption = "Test caption",
				hasAudio = ImageInfo.HasAudio.NO_AUDIO,
			)
		)
	}
}
