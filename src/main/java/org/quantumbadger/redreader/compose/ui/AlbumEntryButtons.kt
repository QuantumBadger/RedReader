package org.quantumbadger.redreader.compose.ui

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.width
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.common.LinkHandler
import org.quantumbadger.redreader.compose.ctx.LocalActivity
import org.quantumbadger.redreader.image.ImageUrlInfo

@Composable
fun AlbumEntryButtons(
	modifier: Modifier = Modifier,
	image: ImageUrlInfo,
) {
	val activity = LocalActivity.current

	Row(
		modifier = modifier,
		// TODO left hand mode
		horizontalArrangement = Arrangement.Absolute.Right
	) {

		RRIconButton(
			onClick = {
				LinkHandler.onActionMenuItemSelected(
					uri = image.url,
					activity = activity,
					action = LinkHandler.LinkAction.SAVE_IMAGE
				)
			},
			icon = R.drawable.download,
			contentDescription = R.string.action_save_image
		)

		RRDropdownMenuIconButton(
			icon = R.drawable.ic_action_share_dark,
			contentDescription = R.string.action_share_image
		) {
			Item(
				icon = R.drawable.ic_action_image_dark,
				text = "Share media",
				onClick = {
					LinkHandler.onActionMenuItemSelected(
						uri = image.url,
						activity = activity,
						action = LinkHandler.LinkAction.SHARE_IMAGE
					)
				}
			)
			Item(
				icon = R.drawable.ic_action_link_dark,
				text = "Share link",
				onClick = {
					LinkHandler.onActionMenuItemSelected(
						uri = image.url,
						activity = activity,
						action = LinkHandler.LinkAction.SHARE
					)
				}
			)
		}

		RRIconButton(
			onClick = {
				LinkHandler.onLinkLongClicked(
					activity = activity,
					uri = image.url,
					forceNoImage = false
				)
			},
			icon = R.drawable.dots_vertical_dark,
			contentDescription = R.string.three_dots_menu
		)

		Spacer(Modifier.width(4.dp))
	}
}
