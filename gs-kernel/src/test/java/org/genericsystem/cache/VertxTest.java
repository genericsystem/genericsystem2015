package org.genericsystem.cache;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

@Test
public class VertxTest extends AbstractTest {

	@BeforeClass
	public void beforeClass() {
		Vertx.vertx().deployVerticle(HttpGSServer.class.getName(), new DeploymentOptions().setConfig(new JsonObject().put("port", 8081)));
	}

	@Test(invocationCount = 20)
	public void test_001() {
		ClientEngine engine = new ClientEngine("coucou");
	}

	@AfterClass
	public void afterClass() {
		Vertx.vertx().undeploy(HttpGSServer.class.getName());
	}
}
