package org.quantumbadger.redreader.common;

public abstract class InterruptableThread {

	private final InnerThread innerThread;

	public InterruptableThread(String name) {
		innerThread = new InnerThread(name);
	}

	public void start() {
		innerThread.start();
	}

	public void stop() {
		innerThread.interrupt();
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
