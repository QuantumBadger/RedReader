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

import androidx.annotation.DrawableRes
import androidx.annotation.StringRes
import androidx.compose.foundation.layout.size
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme

@Composable
fun RRIconButton(
	onClick: () -> Unit,
	@DrawableRes icon: Int,
	@StringRes contentDescription: Int,
	modifier: Modifier = Modifier,
	tint: Color? = null,
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
			tint = tint ?: theme.postCard.iconColor
		)
	}
}
