package org.genericsystem.cache;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.kernel.Generic;
import org.genericsystem.kernel.ServerCache;
import org.genericsystem.kernel.ServerEngine;
import org.genericsystem.kernel.Statics;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class VertxTest2 extends AbstractTest {

	String ServerVerticleId;

	@BeforeClass
	public void beforeClass() {

		BlockingQueue<String> queue = new ArrayBlockingQueue<>(1);
		GSVertx.vertx()
				.getVertx()
				.deployVerticle(HttpGSServer.class.getName(),
						new GSDeploymentOptions(), result -> {
							try {
								queue.put(result.result());
							} catch (Exception e1) {
								e1.printStackTrace();
							}
							;
						});
		try {
			ServerVerticleId = queue.take();
		} catch (InterruptedException e) {
			e.printStackTrace();
			return;
		}
		System.out.println("beforeClass ok");
	}

	@Test(invocationCount = 10)
	public void test_001() {
		ClientEngine engine = new ClientEngine();
	}

	// @Test
	public void test_003() {

		ClientEngine engine = new ClientEngine("database1");
		ClientEngine engine2 = new ClientEngine(Statics.ENGINE_VALUE);
	}

	// @Test
	public void test_004() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric myVehicle = engine.addInstance("Vehicle2");
		engine.getCurrentCache().flush();
		engine.close();
		afterClass();
		beforeClass();
		ClientEngine secondEngine = new ClientEngine();
		assert secondEngine.getInstance("Vehicle2") != null;
	}

	// @Test
	public void test001_() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric myCar1 = car.addInstance("myCar1");
		ClientCache cache1 = engine.getCurrentCache();
		cache1.flush();
		myCar1.remove();
		cache1.flush();
		ClientCache cache2 = engine.newCache().start();
		catchAndCheckCause(() -> myCar1.remove(),
				AliveConstraintViolationException.class);
		cache2.flush();
	}

	@Test
	public void test_002() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric myVehicle = engine.addInstance("Vehicle");
		engine.getCurrentCache().flush();
		ClientEngine secondEngine = new ClientEngine();
		secondEngine.newCache().start();
		assert myVehicle.isAlive();
		assert secondEngine.getInstance("Vehicle") != null;
	}

	@Test
	public void testConcurentRemoveKO() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache().start();
		final ClientGeneric car = engine.addInstance("Car");
		cache.flush();

		ClientEngine engine2 = new ClientEngine();
		ClientCache cache2 = engine2.newCache().start();

		ClientGeneric car2 = engine2.getInstance("Car");
		engine.getCurrentCache().start();
		car.remove();
		assert !engine.getCurrentCache().isAlive(car);
		assert !engine.getInstances().contains(car);

		engine2.getCurrentCache().start();
		assert engine2.getCurrentCache().isAlive(car2);
		assert !engine.getInstances().contains(car);

		engine.getCurrentCache().start();
		engine.getCurrentCache().flush();

		engine2.getCurrentCache().start();

		catchAndCheckCause(() -> {
			car2.remove();
			engine2.getCurrentCache().flush();
		}, OptimisticLockConstraintViolationException.class);

	}

	@Test
	public void testConcurrencyControlException() {
		ServerEngine engine = new ServerEngine();
		ServerCache cache = engine.newCache().start();
		final Generic car = engine.addInstance("Car");
		cache.flush();

		ServerCache cache2 = engine.newCache().start();
		assert cache2.isAlive(car);
		assert engine.getInstances().contains(car);

		cache.start();
		car.remove();
		assert !cache.isAlive(car);
		assert !engine.getInstances().contains(car);

		cache2.start();
		assert cache2.isAlive(car);
		assert engine.getInstances().contains(car);

		cache.start();

		try {
			cache.tryFlush();
			throw new IllegalStateException();
		} catch (ConcurrencyControlException ex) {
			// ignore
		}

	}

	@AfterClass
	public void afterClass() {
		BlockingQueue<Integer> queue = new ArrayBlockingQueue<>(1);
		GSVertx.vertx().getVertx().undeploy(ServerVerticleId, result -> {
			try {
				queue.put(0);
			} catch (Exception e1) {
				e1.printStackTrace();
			}
		});
		try {
			queue.take();
		} catch (InterruptedException e) {
			e.printStackTrace();
			return;
		}
		System.out.println("afterClass ok");
	}

}
