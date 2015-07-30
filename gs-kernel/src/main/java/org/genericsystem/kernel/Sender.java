//package org.genericsystem.kernel;
//
//import io.vertx.core.AbstractVerticle;
//import io.vertx.core.eventbus.EventBus;
//import io.vertx.example.util.ExampleRunner;
//
///*
// * @author <a href="http://tfox.org">Tim Fox</a>
// */
//public class Sender extends AbstractVerticle {
//
//	private static final String CORE_EXAMPLES_DIR = "gs-kernel";
//	private static final String CORE_EXAMPLES_JAVA_DIR = CORE_EXAMPLES_DIR + "/src/main/java/";
//
//	// Convenience method so you can run it in your IDE
//	public static void main(String[] args) {
//		ExampleRunner.runJavaExample(CORE_EXAMPLES_JAVA_DIR, Sender.class, true);
//	}
//
//	@Override
//	public void start() throws Exception {
//		EventBus eb = vertx.eventBus();
//
//		// Send a message every second
//
//		vertx.setPeriodic(1000, v -> {
//
//			eb.send("ping-address", "ping!", reply -> {
//				if (reply.succeeded()) {
//					System.out.println("Received reply " + reply.result().body());
//				} else {
//					System.out.println("No reply");
//				}
//			});
//
//		});
//	}
// }
