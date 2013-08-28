package org.quantumbadger.redreader.common;

import android.os.SystemClock;

import java.util.PriorityQueue;

// TODO store in RRContext
// TODO handle pause/resume from activity
public final class RRSchedulerManager {

	private final Object lock = new Object();
	private volatile int runId = 0;

	private final PriorityQueue<RRSingleTaskScheduler> scheduled = new PriorityQueue<RRSingleTaskScheduler>();

	private RRSchedulerThread thread;

	public void onResume() {
		synchronized(lock) {
			runId++;
			thread = new RRSchedulerThread();
			thread.start();
		}
	}

	public void onPause() {
		synchronized(lock) {
			runId++;
			thread.interrupt();
		}
	}

	public RRSingleTaskScheduler obtain() {
		return new RRSingleTaskScheduler();
	}

	private final class RRSchedulerThread extends Thread {

		private final int thisRunId = runId;

		public RRSchedulerThread() {
			super("RRSchedulerThread " + runId);
		}

		@Override
		public void run() {
			synchronized(lock) {
				while(runId == thisRunId) {

					while(scheduled.isEmpty() || !scheduled.peek().readyToRun()) {
						try {
							if(scheduled.isEmpty()) {
								lock.wait();
							} else {
								lock.wait(scheduled.peek().timeUntilRun());
							}
						} catch(InterruptedException e) {
							return;
						}
					}

					scheduled.remove().run();
				}
			}
		}
	}

	public final class RRSingleTaskScheduler implements Comparable<RRSingleTaskScheduler> {

		private Runnable task;
		private long runtime = -1;
		private boolean runOnUiThread;

		private RRSingleTaskScheduler() {}

		public void setSchedule(final Runnable task, final long delay) {
			setSchedule(task, delay, false);
		}

		public void setSchedule(final Runnable task, final long delay, final boolean runOnUiThread) {
			synchronized(lock) {
				if(this.task != null) throw new UnexpectedInternalStateException();
				this.task = task;
				runtime = SystemClock.uptimeMillis() + delay;
				this.runOnUiThread = runOnUiThread;
				scheduled.offer(this);
				lock.notifyAll();
			}
		}

		private void run() {

			final Runnable task;

			synchronized(lock) {
				if(this.task == null) throw new UnexpectedInternalStateException();
				task = this.task;
				this.task = null;
			}

			if(runOnUiThread) {
				General.runOnUiThread(task);
			} else {
				task.run();
			}
		}

		public void cancel() {
			synchronized(lock) {
				if(task != null) {
					task = null;
					if(!scheduled.remove(this)) throw new UnexpectedInternalStateException();
					lock.notifyAll();
				}
			}
		}

		public long timeUntilRun() {
			synchronized(lock) {
				if(task == null) throw new UnexpectedInternalStateException();
				return runtime - SystemClock.uptimeMillis();
			}
		}

		public boolean readyToRun() {
			return timeUntilRun() < 10;
		}

		public int compareTo(final RRSingleTaskScheduler another) {
			if(task == null) {
				if(another.task == null) {
					return 0;
				} else {
					return 1;
				}
			} else {
				if(another.task == null) {
					return -1;
				} else {
					return (int) Math.min(1, Math.max(-1, runtime - another.runtime));
				}
			}
		}
	}
}
