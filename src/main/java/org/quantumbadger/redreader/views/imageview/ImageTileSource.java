/*******************************************************************************
 * This file is part of RedReader.
 * <p/>
 * RedReader is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * <p/>
 * RedReader is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * <p/>
 * You should have received a copy of the GNU General Public License
 * along with RedReader.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.quantumbadger.redreader.views.imageview;

import android.graphics.Bitmap;

public interface ImageTileSource {
	int getWidth();

	int getHeight();

	int getHTileCount();

	int getVTileCount();

	int getTileSize();

	Bitmap getTile(int sampleSize, int tileX, int tileY);

	void dispose();
}
