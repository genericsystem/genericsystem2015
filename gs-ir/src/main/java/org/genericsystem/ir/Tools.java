package org.genericsystem.ir;

import java.util.function.Consumer;

import com.hazelcast.config.Config;

import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.spi.cluster.ClusterManager;
import io.vertx.spi.cluster.hazelcast.HazelcastClusterManager;

public class Tools {

	public static void deployOnCluster(Consumer<Vertx> deployer) {
		Config hazelcastConfig = new Config();
		hazelcastConfig.setProperty("hazelcast.logging.type", "slf4j");
		ClusterManager mgr = new HazelcastClusterManager(hazelcastConfig);
		VertxOptions vertxOptions = new VertxOptions().setClustered(true).setClusterManager(mgr);
		vertxOptions.setClusterHost(LocalNet.getIpAddress());
		vertxOptions.setMaxWorkerExecuteTime(Long.MAX_VALUE);
		Vertx.clusteredVertx(vertxOptions, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
			Vertx vertx = res.result();
			deployer.accept(vertx);
		});
	}
}
