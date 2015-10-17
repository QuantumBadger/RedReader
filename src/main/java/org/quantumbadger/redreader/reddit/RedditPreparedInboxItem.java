/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package com.konneh.scroll.reddit;

import android.app.Activity;
import android.text.SpannableStringBuilder;
import android.view.ViewGroup;

public interface RedditPreparedInboxItem {

	public SpannableStringBuilder getHeader();

	public ViewGroup getBody(Activity activity, float textSize, Integer textCol, boolean showLinkButtons);

	public void handleInboxClick(Activity activity);

}
