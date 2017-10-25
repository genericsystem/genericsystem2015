package org.genericsystem.cv.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Utility class that can be used to run multiple tasks in parallel.
 * 
 * @author Nicolas Feybesse
 * @author Pierrik Lassalas
 */
public class ParallelTasks {
	private final List<Runnable> tasks;
	private final int counter;

	/**
	 * Default constructor.
	 */
	public ParallelTasks() {
		this(new ArrayList<>());

	}

	/**
	 * Construct a ParallelTasks with a pre-defined list of {@link Runnable}.
	 * 
	 * @param tasks - a list of {@link Runnable}
	 */
	public ParallelTasks(List<Runnable> tasks) {
		this.tasks = tasks;
		this.counter = Runtime.getRuntime().availableProcessors();
	}

	/**
	 * Add a single task to the tasks list.
	 * 
	 * @param task - a {@link Runnable}
	 */
	public void add(final Runnable task) {
		tasks.add(task);
	}

	/**
	 * Run all the tasks in parallel.
	 * 
	 * @throws InterruptedException if interrupted
	 */
	public void run() throws InterruptedException {
		final ExecutorService threads = Executors.newFixedThreadPool(counter);
		// new ThreadPoolExecutor(counter, 2 * counter, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<Runnable>(), new ThreadPoolExecutor.DiscardOldestPolicy());
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

	/**
	 * The internal Threadpool size (number of available processors)
	 * 
	 * @return the number of available processors
	 */
	public int getCounter() {
		return counter;
	}
}