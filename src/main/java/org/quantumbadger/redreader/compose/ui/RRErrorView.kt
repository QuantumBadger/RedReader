package org.quantumbadger.redreader.compose.ui

import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxHeight
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Icon
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import androidx.constraintlayout.compose.ConstraintLayout
import androidx.constraintlayout.compose.Dimension
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.common.RRError
import org.quantumbadger.redreader.common.invokeIf
import org.quantumbadger.redreader.compose.ctx.Dest
import org.quantumbadger.redreader.compose.ctx.LocalLauncher
import org.quantumbadger.redreader.compose.ctx.RRComposeContextTest
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme

@Composable
fun RRErrorView(error: RRError) {

	val theme = LocalComposeTheme.current.error
	val launch = LocalLauncher.current

	var size by remember { mutableStateOf(IntSize.Zero) }

	val onClick = {
		launch(Dest.ErrorPropertiesDialog(error))
	}

	Box(
		modifier = Modifier
			.padding(8.dp)
			.fillMaxWidth()
			.onSizeChanged { size = it },
		contentAlignment = Alignment.Center
	) {
		val smallWidth = with(LocalDensity.current) {
			size.width.toDp() < 150.dp
		}

		Box(
			Modifier
				.fillMaxWidth()
				.border(1.dp, theme.border, RoundedCornerShape(6.dp))
				.clip(RoundedCornerShape(6.dp))
				.background(theme.background)
				.padding(12.dp)
				.invokeIf(smallWidth) {
					clickable(onClick = onClick).fillMaxHeight()
				},
			contentAlignment = Alignment.Center
		) {
			if (smallWidth) {
				Icon(
					painter = painterResource(id = R.drawable.alert_circle_outline),
					contentDescription = "Error", // TODO string
					tint = theme.border,
				)

			} else {
				ConstraintLayout(Modifier.fillMaxWidth()) {

					val (icon, text) = createRefs()

					Icon(
						painter = painterResource(id = R.drawable.alert_circle_outline),
						contentDescription = null,
						modifier = Modifier
							.constrainAs(icon) {
								start.linkTo(parent.start)
								top.linkTo(parent.top)
							},
						tint = theme.border
					)

					Column(
						Modifier
							.constrainAs(text) {
								start.linkTo(icon.end, 16.dp)
								end.linkTo(parent.end)
								top.linkTo(parent.top)
								bottom.linkTo(parent.bottom)
								width = Dimension.fillToConstraints
							}
					) {

						Text(
							text = error.title ?: "Error",  // TODO string
							style = theme.title
						)

						error.message?.let {
							Spacer(Modifier.height(3.dp))
							Text(text = it, style = theme.message)
						}

						Spacer(Modifier.height(14.dp))

						Row(
							Modifier.fillMaxWidth(),
							horizontalArrangement = Arrangement.End
						) {
							RRButton(
								onClick = onClick,
								text = "Details", // TODO string
								theme = theme.detailsButton
							)
						}
					}
				}
			}
		}
	}
}

@Composable
@Preview
private fun PreviewRRErrorView() {
	RRComposeContextTest {
		RRErrorView(
			error = RRError(
				title = "Error title",
				message = "Error message"
			)
		)
	}
}

@Composable
@Preview(widthDp = 64, heightDp = 64)
private fun PreviewRRErrorViewSmall() {
	RRComposeContextTest {
		RRErrorView(
			error = RRError(
				title = "Error title",
				message = "Error message"
			)
		)
	}
}
