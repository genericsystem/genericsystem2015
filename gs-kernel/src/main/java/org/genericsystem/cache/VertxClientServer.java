package org.genericsystem.cache;

import io.vertx.core.Vertx;

import java.util.concurrent.CountDownLatch;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Server;

public class VertxClientServer implements Server {
	private final ClientEngine engine;
	private final Vertx vertx;

	public VertxClientServer(ClientEngine engine, Vertx vertx) {
		this.engine = engine;
		this.vertx = vertx;
	}

	@Override
	public Vertex getVertex(long id) {
		final CountDownLatch countDownLatch = new CountDownLatch(1);
		Vertex[] resultHandler = new Vertex[1];
		vertx.eventBus().send("getVertex", id, reply -> {
			if (reply.succeeded()) {
				resultHandler[0] = (Vertex) reply.result().body();
				countDownLatch.countDown();
			} else {
				engine.getCurrentCache().discardWithException(reply.cause());
			}
		});
		try {
			countDownLatch.await();
		} catch (InterruptedException e) {
			engine.getCurrentCache().discardWithException(e);
		}
		return resultHandler[0];
	}

	@Override
	public long[] getDependencies(long ts, long id) {
		final CountDownLatch countDownLatch = new CountDownLatch(1);
		long[][] resultHandler = new long[1][];
		vertx.eventBus().send("getDependencies", id, reply -> {
			if (reply.succeeded()) {
				resultHandler[0] = (long[]) reply.result().body();
				countDownLatch.countDown();
			} else {
				engine.getCurrentCache().discardWithException(reply.cause());
			}
		});
		try {
			countDownLatch.await();
		} catch (InterruptedException e) {
			engine.getCurrentCache().discardWithException(e);
		}
		return resultHandler[0];
	}

	@Override
	public void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		final CountDownLatch countDownLatch = new CountDownLatch(1);
		vertx.eventBus().send("apply", removes + adds, reply -> {
			if (reply.succeeded()) {
				countDownLatch.countDown();
			} else {
				engine.getCurrentCache().discardWithException(reply.cause());
			}
		});
		try {
			countDownLatch.await();
		} catch (InterruptedException e) {
			engine.getCurrentCache().discardWithException(e);
		}
	}

	@Override
	public long pickNewTs() {
		long[] resultHandler = new long[1];
		final CountDownLatch countDownLatch = new CountDownLatch(1);
		vertx.eventBus().send("pickNewTs", null, reply -> {
			if (reply.succeeded()) {
				resultHandler[0] = (long) reply.result().body();
				countDownLatch.countDown();
			} else {
				engine.getCurrentCache().discardWithException(reply.cause());
			}
		});
		return resultHandler[0];
	}

	@Override
	public void close() {
		final CountDownLatch countDownLatch = new CountDownLatch(1);
		vertx.eventBus().send("close", null, reply -> {
			if (reply.succeeded()) {
				countDownLatch.countDown();
			} else {
				engine.getCurrentCache().discardWithException(reply.cause());
			}
		});
		try {
			countDownLatch.await();
		} catch (InterruptedException e) {
			engine.getCurrentCache().discardWithException(e);
		}
	}
}
