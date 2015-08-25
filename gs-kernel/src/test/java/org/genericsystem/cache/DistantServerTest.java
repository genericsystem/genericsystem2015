package org.genericsystem.cache;

import io.vertx.core.Vertx;

import org.genericsystem.kernel.Statics;
import org.testng.annotations.Test;

public class DistantServerTest extends AbstractTest {

	Vertx vertx = Vertx.vertx();
	Vertx vertxServer = Vertx.vertx();
	String ServerVerticleId;

	@Test(invocationCount = 100)
	public void test_001() {
		assert vertx != null;
		ClientEngine engine = new ClientEngine(vertx, Statics.ENGINE_VALUE, "82.240.164.123", 8082);
	}

}
