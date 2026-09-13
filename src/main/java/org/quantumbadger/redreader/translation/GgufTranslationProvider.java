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

package org.quantumbadger.redreader.translation;

import android.os.Build;

import java.io.IOException;
import java.io.InterruptedIOException;
import java.nio.charset.StandardCharsets;
import java.util.function.BooleanSupplier;

/** HY-MT2 prompt adapter backed by the embedded llama.cpp runtime. */
public final class GgufTranslationProvider implements TranslationProvider {

	private final TranslationModelStore store;
	private boolean closed;

	public GgufTranslationProvider(final TranslationModelStore store) {
		this.store = store;
	}

	@Override
	public String translate(
			final TranslationRequest request,
			final BooleanSupplier isCancelled) throws IOException {

		if(closed) {
			throw new IllegalStateException("Translation provider is closed");
		}
		if(Build.VERSION.SDK_INT < Build.VERSION_CODES.M) {
			throw new IOException("Local translation requires Android 6.0 or later");
		}
		if(isCancelled.getAsBoolean()) {
			throw new InterruptedIOException("Translation cancelled");
		}
		final String language;
		switch(request.getTargetLanguage()) {
			case "zh-Hans": language = "Simplified Chinese"; break;
			case "zh-Hant": language = "Traditional Chinese"; break;
			case "en": language = "English"; break;
			case "ja": language = "Japanese"; break;
			case "ko": language = "Korean"; break;
			case "fr": language = "French"; break;
			case "de": language = "German"; break;
			case "es": language = "Spanish"; break;
			default: throw new IOException("Unsupported target language");
		}
		final String prompt = "Translate the following text into " + language
				+ ". Output only the translated result, without explanations."
				+ " Preserve Markdown formatting, code and URLs.\n\n" + request.getText();
		// One model runs at a time; native resources are released on every exit path.
		synchronized(store) {
			final byte[] path = store.requireModel().getAbsolutePath()
					.getBytes(StandardCharsets.UTF_8);
			try {
				final byte[] result = LlamaNative.generate(
						path,
						prompt.getBytes(StandardCharsets.UTF_8),
						new LlamaNative.Cancellation(isCancelled));
				return new String(result, StandardCharsets.UTF_8);
			} catch(final UnsatisfiedLinkError error) {
				throw new IOException("The local translation runtime is unavailable", error);
			}
		}
	}

	@Override
	public void close() {
		closed = true;
	}
}
