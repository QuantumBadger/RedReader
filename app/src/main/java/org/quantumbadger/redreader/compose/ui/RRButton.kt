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

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.padding
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.semantics.Role
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import org.quantumbadger.redreader.compose.ctx.RRComposeContextTest
import org.quantumbadger.redreader.compose.theme.ComposeThemeButton
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme
import org.quantumbadger.redreader.compose.theme.StyledText

@Composable
fun RRButton(
	modifier: Modifier = Modifier,
	onClick: () -> Unit,
	text: String,
	theme: ComposeThemeButton
) {
	Box(
		modifier = modifier
			.clickable(onClick = onClick, role = Role.Button)
			.semantics(mergeDescendants = true) {}
			.clip(theme.shape)
			.background(theme.background)
			.padding(horizontal = 16.dp, vertical = 8.dp),
		contentAlignment = Alignment.Center
	) {
		theme.text.StyledText(text)
	}
}

@Composable
@Preview
private fun PreviewRRButton() {
	RRComposeContextTest {

		val theme = LocalComposeTheme.current

		RRButton(
			onClick = { },
			text = "Test Button",
			theme = theme.error.primaryButton
		)
	}
}
