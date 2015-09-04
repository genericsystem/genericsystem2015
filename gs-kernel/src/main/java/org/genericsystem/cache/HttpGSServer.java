package org.genericsystem.cache;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerOptions;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.kernel.Root;

public class HttpGSServer extends AbstractGSServer {

	private List<HttpServer> httpServers = new ArrayList<>();
	private final int port;
	private final String host;

	public HttpGSServer(GSDeploymentOptions options) {
		super(options);
		this.port = options.getPort();
		this.host = options.getHost();
	}

	public void start() {
		Vertx vertx = Vertx.vertx();
		for (int i = 0; i < 2 * Runtime.getRuntime().availableProcessors(); i++) {
			HttpServer httpServer = vertx.createHttpServer(new HttpServerOptions().setPort(port).setHost(host));
			httpServer.requestHandler(request -> {
				String path = request.path();
				Root root = roots.get(path);
				if (root == null)
					throw new IllegalStateException("Unable to find database :" + path);
				request.exceptionHandler(e -> {
					e.printStackTrace();
					throw new IllegalStateException(e);
				});
				request.handler(getHandler(root, buffer -> {
					request.response().end(buffer);
					request.response().close();
				}, exception -> {
					int statusCode = 0;
					if (exception instanceof ConcurrencyControlException)
						statusCode = 400;
					else if (exception instanceof OptimisticLockConstraintViolationException)
						statusCode = 401;
					request.response().setStatusCode(statusCode).end(Buffer.buffer().appendInt(AbstractGSClient.APPLY));
					request.response().close();
				}));
			});
			HttpGSServer.<HttpServer> synchonizeTask(handler -> httpServer.listen(handler));
			httpServers.add(httpServer);
		}
		System.out.println("Generic System server ready!");
	}

	@Override
	public void stop() {
		httpServers.forEach(httpServer -> HttpGSServer.<Void> synchonizeTask(handler -> httpServer.close(handler)));
		super.stop();
		System.out.println("Generic System server stopped!");
	}

	private static <T> T synchonizeTask(Handler<Handler<AsyncResult<T>>> consumer) {
		BlockingQueue<AsyncResult<T>> blockingQueue = new ArrayBlockingQueue<>(1);
		consumer.handle(res -> {
			try {
				blockingQueue.put(res);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		});
		AsyncResult<T> res = null;
		try {
			res = blockingQueue.take();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		if (res.failed())
			throw new IllegalStateException(res.cause());
		return res.result();
	}
}
