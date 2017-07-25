package org.genericsystem.watch;

import org.genericsystem.common.GSVertx;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.watch.gui.WatchApp;

import io.vertx.core.AbstractVerticle;

/**
 * Test Verticle to evaluate whether the GUI application can be launched from a verticle.
 * 
 * @author Pierrik Lassalas
 */
public class TestVerticle extends AbstractVerticle {
	private static final String gsPath = "/gs-cv_model3";

	public static void main(String[] mainArgs) {
		GSVertx.vertx().getVertx().deployVerticle(new TestVerticle(), res -> {
			if (res.failed())
				throw new IllegalStateException("Deployment of worker verticle failed.", res.cause());
		});
	}

	@Override
	public void start() throws Exception {
		String[] mainArgs = {};
		GSVertx.vertx().getVertx().executeBlocking(handler -> {
			ApplicationServer server = ApplicationServer.startSimpleGenericApp(mainArgs, WatchApp.class, gsPath);
			handler.complete(server);
		}, res -> {
			if (res.failed())
				throw new IllegalStateException("Unable to execute the task");
			System.out.println("server: " + res.result());
		});

	}

}
