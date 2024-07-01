package org.quantumbadger.redreader.compose.ui

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.Text
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

@Composable
fun RRButton(
	modifier: Modifier = Modifier,
	onClick: () -> Unit,
	text: String,
	theme: ComposeThemeButton
) {
	// TODO accessibility: test mergeDescendants
	Box(
		modifier = modifier
			.clickable(onClick = onClick, role = Role.Button)
			.semantics(mergeDescendants = true) {}
			.clip(theme.shape)
			.background(theme.background)
			.padding(horizontal = 16.dp, vertical = 8.dp),
		contentAlignment = Alignment.Center
	) {
		Text(
			text = text,
			style = theme.text
		)
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
