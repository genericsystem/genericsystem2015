package org.genericsystem.cache;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

import org.genericsystem.kernel.Statics;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class RemoteClientTest extends AbstractTest {

	@BeforeClass
	public void beforeClass() {
		HttpGSServer.create();

	}

	private interface HttpGSServer {

		public static void close() {
			Vertx.vertx().undeploy(HttpGSClient.class.getName());
		};

		public static void create(int port, String persistanceRepositoryPath) {
			Vertx.vertx().deployVerticle(
					HttpGSClient.class.getName(),
					new DeploymentOptions().setConfig(new JsonObject().put(
							"port", port).put("persistanceRepositoryPath",
							persistanceRepositoryPath)));
		}

		public static void create() {
			create(Statics.DEFAULT_PORT, null);
		}

	}

	@AfterClass
	public void afterClass() {
		HttpGSServer.close();
	}

	@Test
	public void test_001() {
		ClientEngine clientEngine = new ClientEngine("firstEngine");
		ClientGeneric myVehicle = clientEngine.addInstance("Vehicle");
		clientEngine.getCurrentCache().flush();
		ClientEngine clientEngine2 = new ClientEngine("secondEngine");
		// ClientGeneric mySecondVehicle = clientEngine2.getInstance("Vehicle");

	}

}
