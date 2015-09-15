package org.genericsystem.distributed;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.stream.Collectors;
import org.genericsystem.kernel.Root;
import org.genericsystem.kernel.Statics;

public abstract class AbstractGSServer {

	protected Map<String, Root> roots;

	public AbstractGSServer(Root... roots) {
		this.roots = Arrays.stream(roots).collect(Collectors.toMap(root -> "/" + root.getValue(), root -> root));
		assert roots.length == this.roots.size();
	}

	public AbstractGSServer(GSDeploymentOptions options) {
		this(getRoots(options));
	}

	private static Root[] getRoots(GSDeploymentOptions options) {
		Set<Root> roots = new HashSet<>();
		if (options.getEngines().isEmpty()) {
			Root defaultRoot = new Root(Statics.ENGINE_VALUE, null, options.getClasses());
			roots.add(defaultRoot);
			System.out.println("Starts engine : " + "/" + Statics.ENGINE_VALUE);
		} else
			for (Entry<String, String> entry : options.getEngines().entrySet()) {
				roots.add(new Root(entry.getKey(), entry.getValue(), options.getClasses()));
				System.out.println("Starts engine : " + "/" + entry.getKey());
			}
		return roots.toArray(new Root[roots.size()]);
	}

	public void stop() {
		roots.values().forEach(root -> root.close());
		roots = null;
	}

	static <T> T synchonizeTask(Handler<Handler<AsyncResult<T>>> consumer) {
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

	public abstract void start();
}
