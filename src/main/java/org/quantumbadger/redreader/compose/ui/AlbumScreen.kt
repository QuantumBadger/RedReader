package org.quantumbadger.redreader.compose.ui

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.foundation.layout.add
import androidx.compose.foundation.layout.asPaddingValues
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.systemBars
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.compose.prefs.LocalComposePrefs
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme
import org.quantumbadger.redreader.image.AlbumInfo

@Composable
fun AlbumScreen(
	album: AlbumInfo
) {
	val prefs = LocalComposePrefs.current
	val theme = LocalComposeTheme.current

	val contentPadding: PaddingValues = WindowInsets(top = 8.dp, bottom = 24.dp)
		.add(WindowInsets.systemBars)
		.asPaddingValues()

	LazyColumn(
		Modifier
			.fillMaxSize()
			.background(theme.postCard.listBackgroundColor),
		contentPadding = contentPadding
	) {
		item {
			Column(
				Modifier.padding(horizontal = 14.dp).fillMaxWidth()
			) {

				Spacer(Modifier.height(6.dp))

				RRIconButton(
					modifier = Modifier.align(Alignment.End),
					icon = R.drawable.ic_settings_dark,
					contentDescription = R.string.options_settings,
				) {
					// TODO
				}

				Spacer(Modifier.height(6.dp))

				Text(
					modifier = Modifier.fillMaxWidth(),
					text = album.title ?: "Album",  // TODO string
					style = theme.album.title
				)
				Spacer(Modifier.height(6.dp))
				Text(
					modifier = Modifier.fillMaxWidth(),
					text = album.description ?: "${album.images.size} image(s)", // TODO string, "s"
					style = theme.album.subtitle
				)

				Spacer(Modifier.height(16.dp))
			}
		}

		items(count = album.images.size) {
			AlbumCard(image = album.images[it])
		}
	}
}
