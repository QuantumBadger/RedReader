package org.quantumbadger.redreader.compose.ui

import androidx.compose.foundation.ExperimentalFoundationApi
import androidx.compose.foundation.border
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.material3.Icon
import androidx.compose.material3.Text
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

@OptIn(ExperimentalFoundationApi::class)
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
			.combinedClickable(
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
			Text(
				text = title,
				style = theme.title
			)
			Spacer(Modifier.height(1.dp))
			Text(
				text = link.value,
				style = theme.subtitle
			)
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
