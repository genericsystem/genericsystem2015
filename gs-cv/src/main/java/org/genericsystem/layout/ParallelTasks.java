package org.genericsystem.layout;

import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class ParallelTasks {
	private final Collection<Runnable> tasks = new ArrayList<>();

	public ParallelTasks() {
	}

	public void add(final Runnable task) {
		tasks.add(task);
	}

	public void go() throws InterruptedException {
		final ExecutorService threads = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
		try {
			final CountDownLatch latch = new CountDownLatch(tasks.size());
			for (final Runnable task : tasks)
				threads.execute(new Runnable() {
					@Override
					public void run() {
						try {
							task.run();
						} finally {
							latch.countDown();
						}
					}
				});
			latch.await();
		} finally {
			threads.shutdown();
		}
	}

	public static void main(final String[] args) throws Exception {
		ParallelTasks tasks = new ParallelTasks();
		final Runnable waitOneSecond = new Runnable() {
			@Override
			public void run() {
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {
				}
			}
		};
		tasks.add(waitOneSecond);
		tasks.add(waitOneSecond);
		tasks.add(waitOneSecond);
		tasks.add(waitOneSecond);
		final long start = System.currentTimeMillis();
		tasks.go();
		System.err.println(System.currentTimeMillis() - start);
	}
}