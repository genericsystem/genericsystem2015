package org.genericsystem.cache;

import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import org.testng.annotations.Test;

@Test
public class VertxTest extends AbstractTest {

	public void test001() {
		ClientEngine clientEngine = new ClientEngine();
	}

	public static void runExample(String exampleDir, String verticleID) {
		System.setProperty("vertx.cwd", exampleDir);
		Vertx.clusteredVertx(new VertxOptions().setClustered(true), res -> {
			if (res.succeeded()) {
				Vertx vertx = res.result();
				try {
					vertx.deployVerticle(verticleID);
				} catch (Throwable t) {
					t.printStackTrace();
				}
			} else {
				res.cause().printStackTrace();
			}
		});
	}

}
