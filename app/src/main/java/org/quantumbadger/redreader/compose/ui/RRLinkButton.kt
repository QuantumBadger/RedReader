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

import androidx.compose.foundation.border
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.material3.Icon
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.semantics.Role
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.constraintlayout.compose.ConstraintLayout
import androidx.constraintlayout.compose.Dimension
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.common.UriString
import org.quantumbadger.redreader.compose.ctx.Dest
import org.quantumbadger.redreader.compose.ctx.LocalLauncher
import org.quantumbadger.redreader.compose.ctx.RRComposeContextTest
import org.quantumbadger.redreader.compose.theme.ComposeThemeLinkButton
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme
import org.quantumbadger.redreader.compose.theme.StyledText
import org.quantumbadger.redreader.compose.theme.combinedClickableWithHaptics

@Composable
fun RRLinkButton(
	title: String,
	link: UriString,
	theme: ComposeThemeLinkButton
) {
	val launch = LocalLauncher.current

	ConstraintLayout(
		modifier = Modifier
			.fillMaxWidth()
			.semantics(mergeDescendants = true) {}
			.border(theme.borderThickness, theme.borderColor, theme.shape)
			.clip(theme.shape)
			.combinedClickableWithHaptics(
				role = Role.Button,
				onClick = { launch(Dest.Link(link)) },
				onLongClick = { launch(Dest.LinkLongClick(link)) }
			)
			.padding(horizontal = 16.dp, vertical = 12.dp),
	) {

		val (linkIcon, textBox, arrowIcon) = createRefs()

		Icon(
			modifier = Modifier
                .size(20.dp)
                .constrainAs(linkIcon) {
                    top.linkTo(parent.top)
                    bottom.linkTo(parent.bottom)
                    start.linkTo(parent.start)
                },
			painter = painterResource(id = R.drawable.ic_action_link_dark),
			contentDescription = null,
			tint = theme.iconColor
		)
		Column(modifier = Modifier.constrainAs(textBox) {
			top.linkTo(parent.top)
			bottom.linkTo(parent.bottom)
			start.linkTo(linkIcon.end, 12.dp)
			end.linkTo(arrowIcon.start, 12.dp)
			width = Dimension.fillToConstraints
		}) {
			theme.title.StyledText(title)
			Spacer(Modifier.height(1.dp))
			theme.subtitle.StyledText(link.value)
		}
		Icon(
			modifier = Modifier
                .size(20.dp)
                .constrainAs(arrowIcon) {
                    top.linkTo(parent.top)
                    bottom.linkTo(parent.bottom)
                    end.linkTo(parent.end)
                },
			painter = painterResource(id = R.drawable.chevron_right_dark),
			contentDescription = null,
			tint = theme.iconColor
		)
	}
}

@Composable
@Preview
private fun PreviewRRLinkButton() {
	RRComposeContextTest {

		val theme = LocalComposeTheme.current

		RRLinkButton(
			title = "Test Button",
			link = UriString("https://redreader.org"),
			theme = theme.linkButton
		)
	}
}
