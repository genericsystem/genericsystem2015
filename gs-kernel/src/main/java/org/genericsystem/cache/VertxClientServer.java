package org.genericsystem.cache;

import io.vertx.core.AsyncResult;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.eventbus.EventBus;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Server;

public class VertxClientServer implements Server {
	private final ClientEngine engine;
	private final EventBus bus;

	VertxClientServer(ClientEngine engine) {
		this.engine = engine;
		this.bus = initVertx().eventBus();
	}

	private Vertx initVertx() {
		System.setProperty("vertx.cwd", "/src/main/java/" + VertxServerClient.class.getPackage().getName().replace(".", "/"));
		CountDownLatch latch = new CountDownLatch(1);
		Vertx[] vertxArray = new Vertx[1];
		Vertx.clusteredVertx(new VertxOptions().setClustered(true), res -> {
			if (res.succeeded()) {
				vertxArray[0] = res.result();
				try {
					vertxArray[0].deployVerticle(VertxServerClient.class.getName());
					latch.countDown();
				} catch (Throwable t) {
					t.printStackTrace();
				}
			} else {
				res.cause().printStackTrace();
			}
		});
		try {
			latch.await();
		} catch (InterruptedException e) {
			throw new IllegalStateException(e);
		}
		return vertxArray[0];
	}

	@Override
	public Vertex getVertex(long id) {
		try {
			System.out.println("Call getVertex with id = " + id);
			return new Vertex((JsonObject) synchronize("getVertex", id));
		} catch (ClassNotFoundException e) {
			throw new IllegalStateException(e);
		}
	}

	@Override
	public Long[] getDependencies(long ts, long id) {
		JsonArray json = synchronize("getDependencies", new JsonObject().put("id", id).put("ts", ts));
		return (Long[]) json.getList().toArray(new Long[json.getList().size()]);
	}

	@SuppressWarnings("unchecked")
	public <T> T synchronize(String methodName, Object parameters) {
		final CountDownLatch countDownLatch = new CountDownLatch(1);
		AsyncResult<?>[] replyArray = new AsyncResult<?>[1];
		bus.send(methodName, parameters, reply -> {
			replyArray[0] = reply;
			countDownLatch.countDown();
		});
		try {
			countDownLatch.await(2000, TimeUnit.MILLISECONDS);
		} catch (InterruptedException e) {
			engine.getCurrentCache().discardWithException(e);;
		}
		if (replyArray[0].succeeded())
			return (T) ((Message<Object>) replyArray[0].result()).body();
		engine.getCurrentCache().discardWithException(replyArray[0].cause());
		return null;// Unreachable
	}

	public static class Apply {
		public long ts;
		public long[] removes;
		public Vertex[] adds;

		Apply(long ts, long[] removes, Vertex[] adds) {
			this.ts = ts;
			this.removes = removes;
			this.adds = adds;
		}
	}

	@Override
	public void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		final CountDownLatch countDownLatch = new CountDownLatch(1);
		bus.send("apply", new Apply(ts, removes, adds), reply -> {
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
		return (Long) synchronize("pickNewTs", null);
	}

	@Override
	public void close() {
		final CountDownLatch countDownLatch = new CountDownLatch(1);
		bus.send("close", null, reply -> {
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
