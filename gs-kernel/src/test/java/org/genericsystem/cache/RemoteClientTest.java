package org.genericsystem.cache;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

import org.genericsystem.kernel.Statics;
import org.testng.annotations.AfterTest;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

public class RemoteClientTest extends AbstractTest {

	@BeforeTest
	public void beforeClass() {
		HttpLocalGSServer.create();

	}

	private interface HttpLocalGSServer {

		public static void close() {
			Vertx.vertx().undeploy(HttpGSServer.class.getName());
		};

		public static void create(int port, String persistanceRepositoryPath) {
			Vertx.vertx().deployVerticle(
					HttpGSServer.class.getName(),
					new DeploymentOptions().setConfig(new JsonObject().put(
							"port", port).put("persistanceRepositoryPath",
							persistanceRepositoryPath)));
		}

		public static void create() {
			create(Statics.DEFAULT_PORT, null);
		}

	}

	@AfterTest
	public void afterClass() {
		HttpLocalGSServer.close();
	}

	@Test(invocationCount = 10)
	public void test_001() {
		ClientEngine clientEngine = new ClientEngine("firstEngine");
		ClientGeneric myVehicle = clientEngine.addInstance("Vehicle");
		// clientEngine.getCurrentCache().flush();
		// ClientEngine clientEngine2 = new ClientEngine("firstEngine");
		// ClientGeneric mySecondVehicle = clientEngine2.getInstance("Vehicle");

	}

}
