package org.genericsystem.cv.application;

import java.lang.reflect.Field;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class BoundedScheduledThreadPoolExecutor extends ScheduledThreadPoolExecutor {
	public BoundedScheduledThreadPoolExecutor(int corePoolSize, RejectedExecutionHandler handler, int queueCapacity) {
		super(corePoolSize, handler);
		setMaximumPoolSize(corePoolSize);
		setKeepAliveTime(0, TimeUnit.MILLISECONDS);
		LinkedBlockingQueue<Runnable> queue = new LinkedBlockingQueue<Runnable>(queueCapacity) {
			@Override
			public boolean add(Runnable r) {
				boolean added = offer(r);
				if (added) {
					return added;
				} else {
					getRejectedExecutionHandler().rejectedExecution(r, null);
					return false;
				}
			}
		};

		try {
			Field workQueueField = ThreadPoolExecutor.class.getDeclaredField("workQueue");
			workQueueField.setAccessible(true);
			workQueueField.set(this, queue);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
}