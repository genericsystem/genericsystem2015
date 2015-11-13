package org.genericsystem.distributed;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;

import java.util.Arrays;
import java.util.HashSet;
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
		this.roots = Arrays.stream(roots).collect(Collectors.toMap(root -> "/" + root.getValue(), root -> root));	
	}*/

	public AbstractGSServer(GSDeploymentOptions options) {
		webSocket = new WebSocketServer<T>(this, options);
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
	
	protected WebSocketServer<T> webSocket;
	
	abstract protected T buildRoot(String value, String persistentDirectoryPath, Class<?>[] userClasses);

	public void start(){
		webSocket.start(roots);
	}
	
	public void stop() {
		webSocket.stop(roots);
	}
	/*
	private void stop() {
		roots.values().forEach(root -> root.close());
		roots = null;
	}
	 */
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
	
	
	
	
}
