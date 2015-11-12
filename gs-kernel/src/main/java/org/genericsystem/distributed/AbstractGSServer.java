package org.genericsystem.distributed;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerOptions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.stream.Collectors;

import org.genericsystem.kernel.AbstractServer;
import org.genericsystem.kernel.Statics;

public abstract class AbstractGSServer <T extends AbstractServer> {
	
	protected Map<String, AbstractServer> roots;

	/*public AbstractGSServer(AbstractServer... roots) {
		System.out.println("Plop!!");
		this.roots = Arrays.stream(roots).collect(Collectors.toMap(root -> "/" + root.getValue(), root -> root));	
	}*/

	public AbstractGSServer(GSDeploymentOptions options) {
		webSocket = new WebSocketServer(options);
		this.roots = Arrays.stream(getRoots(options)).collect(Collectors.toMap(root -> "/" + root.getValue(), root -> root));
	}

	private AbstractServer[] getRoots(GSDeploymentOptions options) {
		Set<AbstractServer> roots = new HashSet<>();
		if (options.getEngines().isEmpty()) {
			AbstractServer defaultRoot = buildRoot(Statics.ENGINE_VALUE, null, options.getClasses());
			roots.add(defaultRoot);
			System.out.println("Starts engine : " + "/" + Statics.ENGINE_VALUE);
		} else
			for (Entry<String, String> entry : options.getEngines().entrySet()) {
				roots.add(buildRoot(entry.getKey(), entry.getValue(), options.getClasses()));
				System.out.println("Starts engine : " + "/" + entry.getKey());
			}
		return roots.toArray(new AbstractServer[roots.size()]);
	}

	abstract protected T buildRoot(String value, String persistentDirectoryPath, Class<?>[] userClasses);

	public void start(){
		webSocket.start();
	}
	
	public void stop() {
		webSocket.stop();
	}
	
	private void stopInt() {
		roots.values().forEach(root -> root.close());
		roots = null;
	}

	protected static <T> T synchronizeTask(Handler<Handler<AsyncResult<T>>> consumer) {
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
	
	protected abstract Buffer getReplyBuffer(int methodId, int op, T root, GSBuffer gsBuffer);
	
	
	
	
	
	protected WebSocketServer webSocket;
	
	protected class WebSocketServer {
		private List<HttpServer> httpServers = new ArrayList<>();
		private final int port;
		private final String host;

		public WebSocketServer(GSDeploymentOptions options) {
			this.port = options.getPort();
			this.host = options.getHost();
		}

		@SuppressWarnings("unchecked")
		public void start() {
			Vertx vertx = GSVertx.vertx().getVertx();
			for (int i = 0; i < 2 * Runtime.getRuntime().availableProcessors(); i++) {
				HttpServer httpServer = vertx.createHttpServer(new HttpServerOptions().setPort(port).setHost(host));
				httpServer.websocketHandler(webSocket -> {
					String path = webSocket.path();
					AbstractServer root = roots.get(path);
					if (root == null)
						throw new IllegalStateException("Unable to find database :" + path);
					webSocket.exceptionHandler(e -> {

						e.printStackTrace();
						throw new IllegalStateException(e);
					});
					webSocket.handler(buffer -> {
						GSBuffer gsBuffer = new GSBuffer(buffer);
						int methodId = gsBuffer.getInt();
						int op = gsBuffer.getInt();
						webSocket.writeBinaryMessage(getReplyBuffer(methodId, op, (T) root, gsBuffer));
					});

				});
				// /!\
				AbstractGSServer.<HttpServer> synchronizeTask(handler -> httpServer.listen(handler));
				httpServers.add(httpServer);
			}
			System.out.println("Generic System server ready!");
		}

		public void stop() {
			// /!\
			httpServers.forEach(httpServer -> AbstractGSServer.<Void> synchronizeTask(handler -> httpServer.close(handler)));
			stopInt();
			System.out.println("Generic System server stopped!");
		}

	}
	
}
