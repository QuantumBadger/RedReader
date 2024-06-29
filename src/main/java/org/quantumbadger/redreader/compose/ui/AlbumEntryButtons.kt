package org.quantumbadger.redreader.compose.ui

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.width
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import org.quantumbadger.redreader.R

@Composable
fun AlbumEntryButtons(modifier : Modifier = Modifier) {
	Row(
		modifier = modifier,
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
