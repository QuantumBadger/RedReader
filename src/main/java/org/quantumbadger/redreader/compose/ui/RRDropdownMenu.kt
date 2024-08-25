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
import androidx.compose.animation.core.LinearOutSlowInEasing
import androidx.compose.animation.core.MutableTransitionState
import androidx.compose.animation.core.animateFloat
import androidx.compose.animation.core.tween
import androidx.compose.animation.core.updateTransition
import androidx.compose.foundation.ScrollState
import androidx.compose.foundation.background
import androidx.compose.foundation.focusable
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ColumnScope
import androidx.compose.foundation.layout.IntrinsicSize
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.sizeIn
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.verticalScroll
import androidx.compose.material3.Checkbox
import androidx.compose.material3.DropdownMenuItem
import androidx.compose.material3.HorizontalDivider
import androidx.compose.material3.Icon
import androidx.compose.material3.RadioButton
import androidx.compose.material3.Slider
import androidx.compose.runtime.Composable
import androidx.compose.runtime.Immutable
import androidx.compose.runtime.MutableState
import androidx.compose.runtime.Stable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.draw.shadow
import androidx.compose.ui.graphics.TransformOrigin
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.semantics.clearAndSetSemantics
import androidx.compose.ui.semantics.contentDescription
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.DpOffset
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.IntRect
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.Popup
import androidx.compose.ui.window.PopupPositionProvider
import androidx.compose.ui.window.PopupProperties
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.compose.prefs.Preference
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme
import org.quantumbadger.redreader.compose.theme.StyledText
import kotlin.math.max
import kotlin.math.min
import kotlin.math.roundToInt

@Composable
fun RRDropdownMenuIconButton(
	modifier: Modifier = Modifier,
	@DrawableRes icon: Int,
	@StringRes contentDescription: Int,
	content: @Composable RRDropdownMenuScope.() -> Unit,
) {
	Box(modifier = modifier) {

		val state = rememberRRDropdownMenuState()

		RRIconButton(
			onClick = { state.expanded = true },
			icon = icon,
			contentDescription = contentDescription,
		)

		RRDropdownMenu(
			state = state,
			content = content
		)
	}
}

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
	fun ItemGroupCollapsible(
		@StringRes text: Int,
		content: @Composable RRDropdownMenuScope.() -> Unit
	) {
		var expanded by remember { mutableStateOf(false) }

		Item(
			text = text,
			icon = if (expanded) {
				R.drawable.chevron_down
			} else {
				R.drawable.chevron_right_dark
			},
			onClick = { expanded = !expanded },
			dismissOnClick = false
		)

		if (expanded) {
			content()
		}
	}

	@Composable
	fun Item(
		@StringRes text: Int,
		@DrawableRes icon: Int? = null,
		radioButtonWithValue: Boolean? = null,
		checkboxWithValue: Boolean? = null,
		onClick: () -> Unit,
		dismissOnClick: Boolean = true
	) {
		RRDropdownMenuItem(
			text = text,
			icon = icon,
			onClick = if (dismissOnClick) {
				{
					menuState.expanded = false
					onClick()
				}
			} else {
				onClick
			},
			radioButtonWithValue = radioButtonWithValue,
			checkboxWithValue = checkboxWithValue,
		)
	}

	@Composable
	fun ItemPrefBool(
		@StringRes text: Int,
		@DrawableRes icon: Int? = null,
		pref: Preference<Boolean>,
		dismissOnClick: Boolean = false
	) {
		Item(
			text = text,
			icon = icon,
			checkboxWithValue = pref.value,
			onClick = { pref.value = !pref.value },
			dismissOnClick = dismissOnClick
		)
	}

	@Composable
	fun <T> ItemPrefRadio(
		pref: Preference<T>,
		dismissOnClick: Boolean = false,
		items: @Composable RRRadioScope<T>.() -> Unit
	) {
		items(RRRadioScope(pref, dismissOnClick))
	}


	@Composable
	fun ItemPrefIntSlider(
		@StringRes text: Int,
		pref: Preference<Int>,
		min: Int,
		max: Int,
		continuous: Boolean
	) {
		val theme = LocalComposeTheme.current

		Column(
			Modifier
				.fillMaxWidth()
				.padding(
					top = 12.dp,
					start = 18.dp,
					end = 18.dp
				)
				.sizeIn(
					minWidth = 112.dp,
					maxWidth = 280.dp,
					minHeight = 48.dp
				)
		) {
			theme.dropdownMenu.text.StyledText(stringResource(text))

			Slider(
				modifier = Modifier
					.fillMaxWidth(),
				value = pref.value.toFloat(),
				onValueChange = { pref.value = it.roundToInt() },
				valueRange = min.toFloat()..max.toFloat(),
				steps = ((max - min) - 1).takeUnless { continuous } ?: 0,
			)
		}
	}

	@Composable
	fun ItemDivider() {
		HorizontalDivider()
	}
}

class RRRadioScope<T>(
	private val pref: Preference<T>,
	private val dismissOnClick: Boolean,
) {
	@Composable
	fun RRDropdownMenuScope.Option(
		value: T,
		@StringRes text: Int,
		@DrawableRes icon: Int? = null,
	) {
		Item(
			text = text,
			icon = icon,
			dismissOnClick = dismissOnClick,
			onClick = { pref.value = value },
			radioButtonWithValue = pref.value == value
		)
	}
}

@Composable
fun RRDropdownMenu(
	state: RRDropdownMenuState,
	content: @Composable RRDropdownMenuScope.() -> Unit,
) {
	BaseDropdownMenu(
		expanded = state.expanded,
		onDismissRequest = { state.expanded = false },
		content = {
			content(RRDropdownMenuScope(state))
		},
	)
}

@Composable
private fun RRDropdownMenuItem(
	@StringRes text: Int,
	@DrawableRes icon: Int?,
	onClick: () -> Unit,
	radioButtonWithValue: Boolean?,
	checkboxWithValue: Boolean?
) {
	val theme = LocalComposeTheme.current

	DropdownMenuItem(
		modifier = Modifier.semantics(mergeDescendants = true) {},
		onClick = onClick,
		text = {
			Row(verticalAlignment = Alignment.CenterVertically) {

				Spacer(Modifier.width(6.dp))

				if (icon != null) {
					Icon(
						painter = painterResource(id = icon),
						contentDescription = null
					)

					Spacer(Modifier.width(12.dp))
				}

				theme.dropdownMenu.text.StyledText(stringResource(text))
			}
		},
		trailingIcon = {
			if (radioButtonWithValue != null) {
				val description = if (radioButtonWithValue) {
					stringResource(R.string.radio_button_selected)
				} else {
					stringResource(R.string.radio_button_not_selected)
				}

				RadioButton(
					modifier = Modifier
                        .clearAndSetSemantics {
                            contentDescription = description
                        }
                        .focusable(false),
					selected = radioButtonWithValue,
					onClick = onClick,
				)
			}

			if (checkboxWithValue != null) {
				val description = if (checkboxWithValue) {
					stringResource(R.string.checkbox_checked)
				} else {
					stringResource(R.string.checkbox_not_checked)
				}

				Checkbox(
					modifier = Modifier
                        .clearAndSetSemantics {
                            contentDescription = description
                        }
                        .focusable(false),
					checked = checkboxWithValue,
					onCheckedChange = { onClick() }
				)
			}
		}
	)
}

// Classes and functions below taken from Compose, and modified to remove unnecessary padding

@Composable
private fun BaseDropdownMenu(
	expanded: Boolean,
	onDismissRequest: () -> Unit,
	modifier: Modifier = Modifier,
	offset: DpOffset = DpOffset(0.dp, 0.dp),
	scrollState: ScrollState = rememberScrollState(),
	properties: PopupProperties = PopupProperties(focusable = true),
	content: @Composable ColumnScope.() -> Unit
) {
	val expandedStates = remember { MutableTransitionState(false) }
	expandedStates.targetState = expanded

	if (expandedStates.currentState || expandedStates.targetState) {
		val transformOriginState = remember { mutableStateOf(TransformOrigin.Center) }
		val density = LocalDensity.current
		val popupPositionProvider = DropdownMenuPositionProvider(
			offset,
			density
		) { parentBounds, menuBounds ->
			transformOriginState.value = calculateTransformOrigin(parentBounds, menuBounds)
		}

		Popup(
			onDismissRequest = onDismissRequest,
			popupPositionProvider = popupPositionProvider,
			properties = properties
		) {
			BaseDropdownMenuContent(
				expandedStates = expandedStates,
				transformOriginState = transformOriginState,
				scrollState = scrollState,
				modifier = modifier,
				content = content
			)
		}
	}
}

@Composable
private fun BaseDropdownMenuContent(
	expandedStates: MutableTransitionState<Boolean>,
	transformOriginState: MutableState<TransformOrigin>,
	scrollState: ScrollState,
	modifier: Modifier = Modifier,
	content: @Composable ColumnScope.() -> Unit
) {
	val theme = LocalComposeTheme.current

	// Menu open/close animation.
	val transition = updateTransition(expandedStates, "DropDownMenu")

	val scale by transition.animateFloat(
		transitionSpec = {
			if (false isTransitioningTo true) {
				// Dismissed to expanded
				tween(
					durationMillis = 120,
					easing = LinearOutSlowInEasing
				)
			} else {
				// Expanded to dismissed.
				tween(
					durationMillis = 1,
					delayMillis = 75 - 1
				)
			}
		}, label = "Dropdown menu open/close"
	) {
		if (it) {
			// Menu is expanded.
			1f
		} else {
			// Menu is dismissed.
			0.8f
		}
	}

	val alpha by transition.animateFloat(
		transitionSpec = {
			if (false isTransitioningTo true) {
				// Dismissed to expanded
				tween(durationMillis = 30)
			} else {
				// Expanded to dismissed.
				tween(durationMillis = 75)
			}
		}, label = "Dropdown menu alpha"
	) {
		if (it) {
			// Menu is expanded.
			1f
		} else {
			// Menu is dismissed.
			0f
		}
	}
	Box(
		modifier = Modifier
            .graphicsLayer {
                scaleX = scale
                scaleY = scale
                this.alpha = alpha
                transformOrigin = transformOriginState.value
            }
            .shadow(3.dp, RoundedCornerShape(6.dp))
            .clip(RoundedCornerShape(6.dp))
            .background(theme.dropdownMenu.background),
	) {
		Column(
			modifier = modifier
                .padding(vertical = 0.dp)
                .width(IntrinsicSize.Max)
                .verticalScroll(scrollState),
			content = content
		)
	}
}

@Immutable
internal data class DropdownMenuPositionProvider(
	val contentOffset: DpOffset,
	val density: Density,
	val onPositionCalculated: (IntRect, IntRect) -> Unit = { _, _ -> }
) : PopupPositionProvider {
	override fun calculatePosition(
		anchorBounds: IntRect,
		windowSize: IntSize,
		layoutDirection: LayoutDirection,
		popupContentSize: IntSize
	): IntOffset {
		// The min margin above and below the menu, relative to the screen.
		val verticalMargin = with(density) { 48.dp.roundToPx() }
		// The content offset specified using the dropdown offset parameter.
		val contentOffsetX = with(density) {
			contentOffset.x.roundToPx() * (if (layoutDirection == LayoutDirection.Ltr) 1 else -1)
		}
		val contentOffsetY = with(density) { contentOffset.y.roundToPx() }

		// Compute horizontal position.
		val leftToAnchorLeft = anchorBounds.left + contentOffsetX
		val rightToAnchorRight = anchorBounds.right - popupContentSize.width + contentOffsetX
		val rightToWindowRight = windowSize.width - popupContentSize.width
		val leftToWindowLeft = 0
		val x = if (layoutDirection == LayoutDirection.Ltr) {
			sequenceOf(
				leftToAnchorLeft,
				rightToAnchorRight,
				// If the anchor gets outside of the window on the left, we want to position
				// toDisplayLeft for proximity to the anchor. Otherwise, toDisplayRight.
				if (anchorBounds.left >= 0) rightToWindowRight else leftToWindowLeft
			)
		} else {
			sequenceOf(
				rightToAnchorRight,
				leftToAnchorLeft,
				// If the anchor gets outside of the window on the right, we want to position
				// toDisplayRight for proximity to the anchor. Otherwise, toDisplayLeft.
				if (anchorBounds.right <= windowSize.width) leftToWindowLeft else rightToWindowRight
			)
		}.firstOrNull {
			it >= 0 && it + popupContentSize.width <= windowSize.width
		} ?: rightToAnchorRight

		// Compute vertical position.
		val topToAnchorBottom = maxOf(anchorBounds.bottom + contentOffsetY, verticalMargin)
		val bottomToAnchorTop = anchorBounds.top - popupContentSize.height + contentOffsetY
		val centerToAnchorTop = anchorBounds.top - popupContentSize.height / 2 + contentOffsetY
		val bottomToWindowBottom = windowSize.height - popupContentSize.height - verticalMargin
		val y = sequenceOf(
			topToAnchorBottom,
			bottomToAnchorTop,
			centerToAnchorTop,
			bottomToWindowBottom
		).firstOrNull {
			it >= verticalMargin &&
					it + popupContentSize.height <= windowSize.height - verticalMargin
		} ?: bottomToAnchorTop

		onPositionCalculated(
			anchorBounds,
			IntRect(x, y, x + popupContentSize.width, y + popupContentSize.height)
		)
		return IntOffset(x, y)
	}
}

private fun calculateTransformOrigin(
	parentBounds: IntRect,
	menuBounds: IntRect
): TransformOrigin {
	val pivotX = when {
		menuBounds.left >= parentBounds.right -> 0f
		menuBounds.right <= parentBounds.left -> 1f
		menuBounds.width == 0 -> 0f
		else -> {
			val intersectionCenter =
				(
						max(parentBounds.left, menuBounds.left) +
								min(parentBounds.right, menuBounds.right)
						) / 2
			(intersectionCenter - menuBounds.left).toFloat() / menuBounds.width
		}
	}
	val pivotY = when {
		menuBounds.top >= parentBounds.bottom -> 0f
		menuBounds.bottom <= parentBounds.top -> 1f
		menuBounds.height == 0 -> 0f
		else -> {
			val intersectionCenter =
				(
						max(parentBounds.top, menuBounds.top) +
								min(parentBounds.bottom, menuBounds.bottom)
						) / 2
			(intersectionCenter - menuBounds.top).toFloat() / menuBounds.height
		}
	}
	return TransformOrigin(pivotX, pivotY)
}
