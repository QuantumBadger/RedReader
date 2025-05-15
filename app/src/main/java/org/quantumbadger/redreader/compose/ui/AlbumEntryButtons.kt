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

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.width
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.compose.ctx.Dest
import org.quantumbadger.redreader.compose.ctx.LocalLauncher
import org.quantumbadger.redreader.image.ImageUrlInfo

@Composable
fun AlbumEntryButtons(
	modifier: Modifier = Modifier,
	image: ImageUrlInfo,
) {
	val launch = LocalLauncher.current

	Row(
		modifier = modifier,
		// TODO left hand mode
		horizontalArrangement = Arrangement.Absolute.Right
	) {

		RRIconButton(
			onClick = {
				launch(Dest.SaveMedia(image.url))
			},
			icon = R.drawable.download,
			contentDescription = R.string.action_save_image
		)

		RRDropdownMenuIconButton(
			icon = R.drawable.ic_action_share_dark,
			contentDescription = R.string.action_share
		) {
			Item(
				icon = R.drawable.ic_action_image_dark,
				text = R.string.action_share_image,
				onClick = {
					launch(Dest.ShareMedia(image.url))
				}
			)
			Item(
				icon = R.drawable.ic_action_link_dark,
				text = R.string.action_share_link,
				onClick = {
					launch(Dest.ShareLink(image.url))
				}
			)
		}

		RRIconButton(
			onClick = {
				launch(Dest.LinkLongClick(image.url))
			},
			icon = R.drawable.dots_vertical_dark,
			contentDescription = R.string.three_dots_menu
		)

		Spacer(Modifier.width(4.dp))
	}
}
