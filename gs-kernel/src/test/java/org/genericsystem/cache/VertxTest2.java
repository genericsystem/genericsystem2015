package org.genericsystem.cache;

import io.vertx.core.Vertx;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import org.genericsystem.kernel.Statics;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class VertxTest2 extends AbstractTest {

	Vertx vertx = Vertx.vertx();
	Vertx vertxServer = Vertx.vertx();
	String ServerVerticleId;

	private final String directoryPath = System.getenv("HOME")
			+ "/test/snapshot_save";

	@BeforeClass
	public void beforeClass() {

		BlockingQueue<String> queue = new ArrayBlockingQueue<>(1);

		vertxServer.deployVerticle(
				HttpGSServer.class.getName(),
				new GSDeploymentOptions().addEngine(Statics.ENGINE_VALUE,
						directoryPath).addEngine("database1", null),
				result -> {
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

	@Test
	public void test_001() {
		assert vertx != null;
		ClientEngine engine = new ClientEngine(vertx);
	}

	@Test
	public void test_002() {
		ClientEngine engine = new ClientEngine(vertx);
		ClientGeneric myVehicle = engine.addInstance("Vehicle");
		engine.getCurrentCache().flush();
		ClientEngine secondEngine = new ClientEngine(vertx);
		assert secondEngine.getInstance("Vehicle") != null;
	}

	@Test
	public void test_003() {

		ClientEngine engine = new ClientEngine(vertx, "database1");
		ClientEngine engine2 = new ClientEngine(vertx, Statics.ENGINE_VALUE);
	}

	@Test
	public void test_004() {
		ClientEngine engine = new ClientEngine(vertx);
		ClientGeneric myVehicle = engine.addInstance("Vehicle2");
		engine.getCurrentCache().flush();
		engine.close();
		afterClass();
		beforeClass();
		ClientEngine secondEngine = new ClientEngine(vertx);
		assert secondEngine.getInstance("Vehicle2") != null;

	}

	@AfterClass
	public void afterClass() {
		BlockingQueue<Integer> queue = new ArrayBlockingQueue<>(1);
		vertxServer.undeploy(ServerVerticleId, result -> {
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
