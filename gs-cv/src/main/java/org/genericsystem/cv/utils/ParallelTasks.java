package org.genericsystem.cv.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class ParallelTasks {
	private final List<Runnable> tasks;
	private final int counter;

	public ParallelTasks() {
		this(new ArrayList<>());

	}

	public ParallelTasks(List<Runnable> tasks) {
		this.tasks = tasks;
		this.counter = Runtime.getRuntime().availableProcessors();
	}

	public void add(final Runnable task) {
		tasks.add(task);
	}

	public void run() throws InterruptedException {
		final ExecutorService threads = Executors.newFixedThreadPool(counter);
		try {
			final CountDownLatch latch = new CountDownLatch(tasks.size());
			tasks.forEach(task -> threads.execute(() -> {
				try {
					task.run();
				} finally {
					latch.countDown();
				}
			}));
			latch.await();
		} finally {
			threads.shutdown();
		}
	}

	public int getCounter() {
		return counter;
	}

	public static void main(final String[] args) throws Exception {
		ParallelTasks tasks = new ParallelTasks();
		final Runnable waitOneSecond = () -> {
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
			}
		};
		tasks.add(waitOneSecond);
		tasks.add(waitOneSecond);
		tasks.add(waitOneSecond);
		tasks.add(waitOneSecond);

		final long start = System.currentTimeMillis();
		tasks.run();
		System.err.println(System.currentTimeMillis() - start);
	}
}