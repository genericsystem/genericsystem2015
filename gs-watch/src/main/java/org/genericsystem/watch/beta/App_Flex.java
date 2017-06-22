package org.genericsystem.watch.beta;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Verticle;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.spi.cluster.ClusterManager;
import io.vertx.spi.cluster.hazelcast.HazelcastClusterManager;

public class App_Flex {

	public static final String STEP1 = "app.AtoBEvents";
	public static final String STEP2 = "app.BtoCEvents";


	public static void deployVerticle(Verticle verticle) {
		
		ClusterManager mgr = new HazelcastClusterManager();
		VertxOptions vertxOptions = new VertxOptions().setClustered(true).setClusterManager(mgr);
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
	
//	public static void main(String[] args) {
//
//		VertxOptions options = new VertxOptions();
//		options.setMaxWorkerExecuteTime(Long.MAX_VALUE);
//		Vertx vertx = Vertx.vertx(options);
//
//		if (args[1].equals("A")) {
//			if (args[0].equals("mount")) {
//				vertx.deployVerticle(new VerticleA(), res -> {
//					if (res.succeeded()) {
//						ID_A = res.result();
//					} else {
//						throw new IllegalStateException("Deployment of verticles A failed.", res.cause());
//					}
//				});
//			} else if (args[0].equals("drop")) {
//				if (!ID_A.equals(null))
//					vertx.undeploy(ID_A);
//			}
//		} else if (args[1].equals("B")) {
//
//			if (args[0].equals("mount")) {
//				vertx.deployVerticle(new VerticleB(), res -> {
//					if (res.succeeded()) {
//						ID_B = res.result();
//					} else {
//						throw new IllegalStateException("Deployment of verticles B failed.", res.cause());
//					}
//				});
//			} else if (args[0].equals("drop")) {
//				if (!ID_B.equals(null))
//					vertx.undeploy(ID_B);
//			}
//		} else if (args[1].equals("C")) {
//
//			if (args[0].equals("mount")) {
//				vertx.deployVerticle(new VerticleC(), res -> {
//					if (res.succeeded()) {
//						ID_C = res.result();
//					} else {
//						throw new IllegalStateException("Deployment of verticles C failed.", res.cause());
//					}
//				});
//
//			} else if (args[0].equals("drop")) {
//				if (!ID_C.equals(null))
//					vertx.undeploy(ID_C);
//			}
//		}
//	}
}
