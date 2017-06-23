package org.genericsystem.watch.beta;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Verticle;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.eventbus.EventBusOptions;
import io.vertx.core.spi.cluster.ClusterManager;
import io.vertx.spi.cluster.hazelcast.HazelcastClusterManager;
import sun.security.x509.CertificateX509Key;

public class App_Flex {

	public static final String STEP1 = "app.AtoBEvents";
	public static final String STEP2 = "app.BtoCEvents";


	public static void deployVerticle(Verticle verticle) {
		
		ClusterManager mgr = new HazelcastClusterManager();
		
		VertxOptions vertxOptions = new VertxOptions().setClustered(true).setClusterManager(mgr);
		vertxOptions.setEventBusOptions(new EventBusOptions()).setClustered(true);		
		
		vertxOptions.setClusterHost("192.168.1.11");
		
		vertxOptions.setMaxWorkerExecuteTime(Long.MAX_VALUE);
		
		
		Vertx.clusteredVertx(vertxOptions, res -> {
			if (res.succeeded()) {
				Vertx vertx = res.result();
				vertx.deployVerticle(verticle, result->{
					System.out.println(result.result());
				});
			} else {
				throw new IllegalStateException(res.cause());
			}
		});
	}	

}
