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

package org.quantumbadger.redreader.reddit.prepared.html;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class MalformedHtmlException extends Exception {

	@NonNull public final String html;
	@Nullable public final Integer charPosition;

	public MalformedHtmlException(
			@NonNull final String message,
			@NonNull final String html,
			@Nullable final Integer charPosition,
			@NonNull final Exception e) {

		super(message, e);
		this.html = html;
		this.charPosition = charPosition;
	}

	public MalformedHtmlException(
			@NonNull final String message,
			@NonNull final String html,
			@Nullable final Integer charPosition) {

		super(message);
		this.html = html;
		this.charPosition = charPosition;
	}
}
