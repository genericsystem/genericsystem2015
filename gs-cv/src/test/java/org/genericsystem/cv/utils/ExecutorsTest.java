package org.genericsystem.cv.utils;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.testng.annotations.Test;

@Test
public class ExecutorsTest {

	private LongTaskOverrider taskOverrider = new LongTaskOverrider();

	@Test
	void testExecutor000() {
		ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor();
		int[] i = new int[] { 0 };
		Runnable command = () -> {
			final int j = i[0];
			taskOverrider.schedule(() -> {
				try {
					Thread.sleep(200);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
				System.out.println(i[0] + " " + j);
			});
			i[0]++;
		};

		executor.scheduleAtFixedRate(command, 0, 100, TimeUnit.MILLISECONDS);

		try {
			Thread.sleep(3000);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		executor.shutdown();
	}

}
