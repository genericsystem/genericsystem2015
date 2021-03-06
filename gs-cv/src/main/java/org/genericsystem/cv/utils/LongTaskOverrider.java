package org.genericsystem.cv.utils;

public class LongTaskOverrider {
	private Runnable isRunning;
	private Runnable waiting;

	public synchronized void schedule(Runnable task) {
		if (isRunning != null) {
			waiting = task;
			System.out.println("Overrides waiting task");
		} else {
			isRunning = task;
			run(isRunning);
		}
	}

	private synchronized void terminate() {
		isRunning = waiting;
		waiting = null;
		if (isRunning != null)
			run(isRunning);
	}

	private void run(Runnable toRun) {
		new Thread() {
			@Override
			public void run() {
				toRun.run();
				terminate();
			};
		}.start();
	}
}
