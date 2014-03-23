package org.quantumbadger.redreader.common;

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
