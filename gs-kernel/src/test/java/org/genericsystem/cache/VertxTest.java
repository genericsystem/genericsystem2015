package org.genericsystem.cache;

import io.vertx.core.Vertx;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import org.genericsystem.kernel.Statics;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

@Test
public class VertxTest extends AbstractTest {

	Vertx vertx = Vertx.vertx();
	Vertx vertxServer = Vertx.vertx();

	String ServerVerticleId;

	@BeforeClass
	public void beforeClass() {
		BlockingQueue<String> queue = new ArrayBlockingQueue<>(1);
		vertxServer.deployVerticle(HttpGSServer.class.getName(), new GSDeploymentOptions(Statics.ENGINE_VALUE), result -> {
			try {
				queue.put(result.result());
			} catch (Exception e1) {
				e1.printStackTrace();
			};
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
		assert vertx != null;
		ClientEngine engine = new ClientEngine(vertx);
	}

	@AfterClass
	public void afterClass() {
		BlockingQueue<Integer> queue = new ArrayBlockingQueue<>(1);
		vertxServer.undeploy(ServerVerticleId, result -> {
			try {
				assert result.succeeded() : result.cause();
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
