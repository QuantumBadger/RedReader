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

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.util.AttributeSet;

public class RRViewWrapperTest extends RRViewWrapper {

	private float[] xPos, yPos;

	final Paint paint = new Paint();

	public RRViewWrapperTest(Context context) {
		super(context);
	}

	public RRViewWrapperTest(Context context, AttributeSet attrs) {
		super(context, attrs);
	}

	public RRViewWrapperTest(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
	}

	@Override
	protected void onTouchEvent(TouchEvent e) {

		switch(e.type) {
			case START:
			case MOVE:
				this.xPos = e.xPos.clone();
				this.yPos = e.yPos.clone();
				break;

			case FINISH:
			case CANCEL:
				xPos = null;
				yPos = null;
		}

		invalidate();
	}

	@Override
	protected void onDraw(Canvas canvas) {

		paint.setColor(Color.GREEN);

		if(xPos != null) {

			for(int i = 0; i < xPos.length; i++) {
				canvas.drawLine(xPos[i], 0, xPos[i], getHeight(), paint);
				canvas.drawLine(0, yPos[i], getWidth(), yPos[i], paint);
			}

		}

	}
}
