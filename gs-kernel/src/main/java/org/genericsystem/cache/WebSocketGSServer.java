//package org.genericsystem.cache;
//
//import io.vertx.core.http.HttpServer;
//import io.vertx.core.http.HttpServerOptions;
//
//import org.genericsystem.kernel.Root;
//
//public class WebSocketGSServer extends AbstractGSServer {
//
//	@Override
//	public void start() {
//		System.out.println("super.start()");
//		super.start();
//		System.out.println("start()");
//		HttpServer httpServer = vertx.createHttpServer(new HttpServerOptions().setPort(config().getInteger("port")));
//		httpServer.websocketHandler(webSocket -> {
//			String path = webSocket.path();
//			Root root = getRoots().get(path);
//			if (root == null)
//				throw new IllegalStateException("Unable to find database :" + path);
//			webSocket.exceptionHandler(e -> {
//				e.printStackTrace();
//			});
//			webSocket.handler(getHandler(root, buffer -> webSocket.writeFinalBinaryFrame(buffer)));
//		});
//		httpServer.listen();
//		System.out.println("Generic System server ready!");
//	}
// }
