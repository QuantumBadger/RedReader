package org.quantumbadger.redreader.compose.ui

import androidx.annotation.DrawableRes
import androidx.annotation.StringRes
import androidx.compose.foundation.layout.size
import androidx.compose.material.Icon
import androidx.compose.material.IconButton
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme

@Composable
fun RRIconButton(
	@DrawableRes icon: Int,
	@StringRes contentDescription: Int,
	modifier: Modifier = Modifier,
	onClick: () -> Unit,
) {
	val theme = LocalComposeTheme.current

	IconButton(
		onClick = onClick,
		modifier = modifier
	) {
		Icon(
			modifier = Modifier.size(24.dp),
			painter = painterResource(id = icon),
			contentDescription = stringResource(id = contentDescription),
			tint = theme.postCard.iconColor
		)
	}
}
