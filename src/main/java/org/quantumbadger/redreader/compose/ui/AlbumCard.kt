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
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.systemBars
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.draw.shadow
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.compose.ctx.RRComposeContextTest
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
            NetImage(
                image = preview,
                maxImageHeight = maxImageHeight
            )

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

            AnimatedVisibility(visible = prefs.albumCardShowButtons.value) {
                Row(
                    Modifier.fillMaxWidth(),
                    // TODO left hand mode
                    horizontalArrangement = Arrangement.Absolute.Right
                ) {

                    RRIconButton(
                        onClick = {},
                        icon = R.drawable.download,
                        contentDescription = R.string.action_save_image
                    )

                    RRIconButton(
                        onClick = {},
                        icon = R.drawable.ic_action_share_dark,
                        contentDescription = R.string.action_share_image
                    )

                    RRIconButton(
                        onClick = {},
                        icon = R.drawable.dots_vertical_dark,
                        contentDescription = R.string.three_dots_menu
                    )

                    Spacer(Modifier.width(4.dp))
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
            ImageInfo(
                original = ImageUrlInfo("testimage", size = ImageSize(100, 100)),
                title = "Test title",
                caption = "Test caption",
                hasAudio = ImageInfo.HasAudio.NO_AUDIO,
            )
        )
    }
}
