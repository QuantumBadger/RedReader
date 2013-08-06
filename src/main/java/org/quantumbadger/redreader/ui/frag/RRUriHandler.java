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

package org.quantumbadger.redreader.ui.frag;

import android.net.Uri;
import android.os.Bundle;
import org.quantumbadger.redreader.ui.RRContext;

public abstract class RRUriHandler {

	public abstract Result handle(RRContext context, Uri uri, Mode mode, Bundle arguments);

	public class Result {

		public final RRFragment fragmentToOpen;

		public Result() {
			this(null);
		}

		public Result(RRFragment fragment) {
			fragmentToOpen = fragment;
		}
	}

	public enum Mode {
		ANY, FORCE_INTERNAL_BROWSER, FORCE_EXTERNAL_BROWSER
	}
}
