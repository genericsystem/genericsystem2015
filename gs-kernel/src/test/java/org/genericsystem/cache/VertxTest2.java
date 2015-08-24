package org.genericsystem.cache;

import io.vertx.core.Vertx;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import org.genericsystem.kernel.Statics;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class VertxTest2 extends AbstractTest {

	Vertx vertx = Vertx.vertx();
	Vertx vertxServer = Vertx.vertx();

	@BeforeClass
	public void beforeClass() {
		BlockingQueue<Integer> queue = new ArrayBlockingQueue<>(1);
		vertxServer.deployVerticle(HttpGSServer.class.getName(),
				new GSDeploymentOptions(Statics.ENGINE_VALUE), result -> {
					try {
						queue.put(0);
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					;
				});
		try {
			queue.take();
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

}
