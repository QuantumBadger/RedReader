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

import java.util.LinkedList;

public class RRSequentialUriHandler extends RRUriHandler {

	private final LinkedList<RRUriHandler> handlers = new LinkedList<RRUriHandler>();

	public void addHandler(RRUriHandler handler) {
		handlers.add(handler);
	}

	@Override
	public Result handle(RRFragmentLayout fragmentManager, Uri uri, Mode mode, Bundle arguments) {

		for(RRUriHandler handler : handlers) {
			final Result r = handler.handle(fragmentManager, uri, mode, arguments);
			if(r != null) return r;
		}

		return null;
	}
}
