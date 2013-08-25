package org.quantumbadger.redreader.common;

public abstract class InterruptableThread {

	private final InnerThread innerThread;
	private volatile boolean isRunning;

	public InterruptableThread(String name) {
		innerThread = new InnerThread(name);
	}

	public void start() {
		isRunning = true;
		innerThread.start();
	}

	public void stop() {
		isRunning = false;
		innerThread.interrupt();
	}

	public boolean isRunning() {
		return isRunning;
	}

	public abstract void run() throws InterruptedException;

	private final class InnerThread extends Thread {

		InnerThread(String name) {
			super(name);
		}

		@Override
		public void run() {
			try {
				InterruptableThread.this.run();
			} catch(InterruptedException ignored) {}
		}
	}
}
