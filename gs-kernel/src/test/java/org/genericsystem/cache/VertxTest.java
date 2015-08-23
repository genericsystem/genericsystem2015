package org.genericsystem.cache;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Vertx;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import org.genericsystem.cache.HttpGSServer.GsDeploymentConfig;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

@Test
public class VertxTest extends AbstractTest {

	Vertx vertx = Vertx.vertx();
	Vertx vertxServer = Vertx.vertx();

	@BeforeClass
	public void beforeClass() {
		BlockingQueue<Integer> queue = new ArrayBlockingQueue<>(1);
		vertxServer.deployVerticle(HttpGSServer.class.getName(), new DeploymentOptions().setConfig(new GsDeploymentConfig()), result -> {
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

	@Test(invocationCount = 1000)
	public void test_001() {
		assert vertx != null;
		ClientEngine engine = new ClientEngine(vertx);
	}

	@AfterClass
	public void afterClass() {
		vertxServer.undeploy(WebSocketGSServer.class.getName());
	}
}
