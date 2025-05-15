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

package org.quantumbadger.redreader.common;

import androidx.annotation.NonNull;

import java.util.concurrent.atomic.AtomicBoolean;

public class RunnableOnce implements Runnable {

	public static final RunnableOnce DO_NOTHING = new RunnableOnce(() -> {});

	private final AtomicBoolean mAlreadyRun = new AtomicBoolean(false);

	@NonNull private final Runnable mRunnable;

	public RunnableOnce(@NonNull final Runnable runnable) {
		mRunnable = runnable;
	}

	@Override
	public final void run() {

		if(!mAlreadyRun.getAndSet(true)) {
			mRunnable.run();
		}
	}
}
