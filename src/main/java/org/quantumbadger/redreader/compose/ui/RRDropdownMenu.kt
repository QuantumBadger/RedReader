package org.quantumbadger.redreader.compose.ui

import androidx.annotation.DrawableRes
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.width
import androidx.compose.material.Checkbox
import androidx.compose.material.Divider
import androidx.compose.material.DropdownMenu
import androidx.compose.material.DropdownMenuItem
import androidx.compose.material.Icon
import androidx.compose.material.RadioButton
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.Stable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.unit.dp
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme

@Stable
class RRDropdownMenuState {
	var expanded by mutableStateOf(false)
}

@Composable
fun rememberRRDropdownMenuState() = remember {
	RRDropdownMenuState()
}

class RRDropdownMenuScope(
	private val menuState: RRDropdownMenuState
) {
	@Composable
	fun Item(
		text: String, // TODO change to StringRes
		@DrawableRes icon: Int? = null,
		radioButtonWithValue: Boolean? = null,
		checkboxWithValue: Boolean? = null,
		onClick: () -> Unit,
	) {
		RRDropdownMenuItem(
			menuState = menuState,
			text = text,
			icon = icon,
			onClick = onClick,
			radioButtonWithValue = radioButtonWithValue,
			checkboxWithValue = checkboxWithValue,
		)
	}

	@Composable
	fun ItemDivider() {
		Divider()
	}
}

@Composable
fun RRDropdownMenu(
	state: RRDropdownMenuState,
	content: @Composable RRDropdownMenuScope.() -> Unit,
) {
	DropdownMenu(
		expanded = state.expanded,
		onDismissRequest = { state.expanded = false },
		content = {
			content(RRDropdownMenuScope(state))
		}
	)
}

@Composable
private fun RRDropdownMenuItem(
	menuState: RRDropdownMenuState,
	text: String, // TODO change to StringRes
	@DrawableRes icon: Int?,
	onClick: () -> Unit,
	radioButtonWithValue: Boolean?,
	checkboxWithValue: Boolean?,
) {
	val theme = LocalComposeTheme.current

	DropdownMenuItem(
		modifier = Modifier.semantics(mergeDescendants = true) {},
		onClick = onClick
	) {
		if (icon != null) {
			Icon(
				painter = painterResource(id = icon),
				contentDescription = null
			)
			Spacer(Modifier.width(12.dp))
		}

		Text(
			text = text,
			style = theme.dropdownMenu.text
		)

		Spacer(Modifier.weight(1f))
		Spacer(Modifier.width(8.dp))

		if (radioButtonWithValue != null) {
			RadioButton(selected = radioButtonWithValue, onClick = onClick)
		}

		if (checkboxWithValue != null) {
			Checkbox(checked = checkboxWithValue, onCheckedChange = { onClick() })
		}
	}
}
