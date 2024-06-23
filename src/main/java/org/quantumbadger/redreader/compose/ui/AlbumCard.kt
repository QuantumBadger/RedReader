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

import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.CircularProgressIndicator
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.draw.shadow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import org.quantumbadger.redreader.account.RedditAccountId
import org.quantumbadger.redreader.common.General
import org.quantumbadger.redreader.compose.ctx.RRComposeContextTest
import org.quantumbadger.redreader.compose.net.NetRequestStatus
import org.quantumbadger.redreader.compose.net.fetchImage
import org.quantumbadger.redreader.compose.prefs.LocalComposePrefs
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme
import org.quantumbadger.redreader.image.ImageInfo
import org.quantumbadger.redreader.image.ImageUrlInfo

@Composable
fun AlbumCard(
	index: Int,
	image: ImageInfo
) {
	val prefs = LocalComposePrefs.current
	val theme = LocalComposeTheme.current

	val preview = image.preview ?: image.original
	val url = General.uriFromString(preview?.url)

	// TODO parse reddit image previews
	// TODO handle videos

	Box(
		modifier = Modifier.fillMaxWidth().padding(horizontal = 12.dp, vertical = 6.dp)
	) {
		Column(
			modifier = Modifier
				.shadow(3.dp, RoundedCornerShape(6.dp))
				.clip(RoundedCornerShape(6.dp))
				.background(theme.postCard.backgroundColor)
		) {
			Box(
				modifier = Modifier.fillMaxWidth(),
				contentAlignment = Alignment.Center,
			) {
				if (preview != null && url != null) {
					val data by fetchImage(
						uri = url,
						user = RedditAccountId(""), // TODO
					)

					when (val data = data) {
						NetRequestStatus.Connecting -> {
							CircularProgressIndicator(
								Modifier.padding(24.dp)
							)
						}

						is NetRequestStatus.Downloading -> {
							// TODO
						}
						is NetRequestStatus.Failed -> {
							// TODO
						}
						is NetRequestStatus.Success -> {
							// TODO
							Image(
								modifier = Modifier.fillMaxWidth(),
								bitmap = data.result,
								contentDescription = null
							)
						}
					}
				}
			}

			Column(
				modifier = Modifier.padding(14.dp)
			) {
				Text(
					text = image.title?.trim()?.takeUnless { it.isEmpty() } ?: "Image ${index + 1}",
					style = theme.postCard.title,
				)

				image.caption?.let { caption ->
					Text(
						text = caption,
						style = theme.postCard.subtitle,
					)
				}
			}
		}
	}
}

@Composable
@Preview(backgroundColor = 0x999999)
fun PreviewAlbumCard() {
	RRComposeContextTest {
		AlbumCard(0, ImageInfo(
			original = ImageUrlInfo("testimage"),
			title = "Test title",
			caption = "Test caption",
			hasAudio = ImageInfo.HasAudio.NO_AUDIO,
		))
	}
}
