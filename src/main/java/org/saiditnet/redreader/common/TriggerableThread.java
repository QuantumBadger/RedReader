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

package org.saiditnet.redreader.common;

public class TriggerableThread {

	private final Runnable task;
	private final long initialDelay;

	private InternalTriggerableThread thread;
	private boolean allowRetrigger = false, shouldRetrigger = false;

	public TriggerableThread(Runnable task, long initialDelay) {
		this.task = task;
		this.initialDelay = initialDelay;
	}

	public synchronized void trigger() {

		if(thread == null) {
			thread = new InternalTriggerableThread();
			thread.start();

		} else if(allowRetrigger) {
			shouldRetrigger = true;
		}
	}

	private synchronized void onSleepEnd() {
		allowRetrigger = true;
	}

	private synchronized boolean shouldThreadContinue() {
		if(shouldRetrigger) {
			shouldRetrigger = false;
			return true;

		} else {
			thread = null;
			allowRetrigger = false;
			return false;
		}
	}

	private final class InternalTriggerableThread extends Thread {
		@Override
		public void run() {
			do {
				try {
					Thread.sleep(initialDelay);
				} catch(InterruptedException e) {
					throw new UnexpectedInternalStateException();
				}

				onSleepEnd();
				task.run();
			} while(shouldThreadContinue());
		}
	}
}
