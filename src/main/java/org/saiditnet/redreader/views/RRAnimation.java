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

package org.saiditnet.redreader.views;

public abstract class RRAnimation implements RRChoreographer.Callback {

	private static final RRChoreographer CHOREOGRAPHER = RRChoreographer.getInstance();

	private long mFirstFrameNanos = -1;

	private boolean mStarted = false;
	private boolean mStopped = false;

	public final void start() {

		if(mStarted) {
			throw new RuntimeException("Attempted to start animation twice!");
		}

		mStarted = true;

		CHOREOGRAPHER.postFrameCallback(this);
	}

	public final void stop() {

		if(!mStarted) {
			throw new RuntimeException("Attempted to stop animation before it's started!");
		}

		if(mStopped) {
			throw new RuntimeException("Attempted to stop animation twice!");
		}

		mStopped = true;
	}

	// Return true to continue animating
	protected abstract boolean handleFrame(final long nanosSinceAnimationStart);

	@Override
	public final void doFrame(final long frameTimeNanos) {

		if(mStopped) {
			return;
		}

		if(mFirstFrameNanos == -1) {
			mFirstFrameNanos = frameTimeNanos;
		}

		if(handleFrame(frameTimeNanos - mFirstFrameNanos)) {
			CHOREOGRAPHER.postFrameCallback(this);
		}
	}
}
