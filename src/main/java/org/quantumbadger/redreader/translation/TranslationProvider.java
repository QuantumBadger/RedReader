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

import java.io.IOException;
import java.util.function.BooleanSupplier;

/**
 * Replaceable translation engine. Call translate and close on the same background
 * worker, never concurrently or on the UI thread. Implementations own model loading,
 * prompts, tokenization and decoding; no Reddit account or access token is required.
 */
public interface TranslationProvider extends AutoCloseable {

	/**
	 * Returns the complete translated text, without prompt echoes or explanations.
	 * Check cancellation during model loading and generation. Cancellation must throw
	 * InterruptedIOException; unsupported input, model errors and output limits must
	 * fail explicitly rather than returning the original text or a partial translation.
	 *
	 * @param request Original text and requested target language.
	 * @param isCancelled Thread-safe cancellation flag supplied by the caller.
	 * @throws IOException If translation cannot be completed.
	 */
	String translate(TranslationRequest request, BooleanSupplier isCancelled) throws IOException;

	/** Releases model memory. The provider must not be used after it is closed. */
	@Override
	void close();
}
