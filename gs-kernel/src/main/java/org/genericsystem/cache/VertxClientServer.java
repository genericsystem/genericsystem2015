package org.genericsystem.cache;

import io.vertx.core.AsyncResult;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.eventbus.EventBus;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.JsonObject;

import java.io.Serializable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Server;

public class VertxClientServer implements Server {

	private final ClientEngine engine;
	private final EventBus bus;
	private static boolean init = false;

	VertxClientServer(ClientEngine engine) {
		this.engine = engine;
		this.bus = initVertx().eventBus();
	}

	private Vertx initVertx() {
		CountDownLatch latch = new CountDownLatch(1);
		Vertx[] vertxArray = new Vertx[1];
		Vertx.clusteredVertx(new VertxOptions().setClustered(true), res -> {
			if (res.succeeded()) {
				vertxArray[0] = res.result();
				try {
					if (!init) {
						vertxArray[0].deployVerticle(VertxServerClient.class.getName());
						init = true;
					}
					vertxArray[0].eventBus().registerDefaultCodec(Long[].class, new SerializableMessageCodec<Long[]>("LongArrayCodec"));
					vertxArray[0].eventBus().registerDefaultCodec(Vertex.class, new SerializableMessageCodec<Vertex>("VertexCodec"));
					vertxArray[0].eventBus().registerDefaultCodec(Apply.class, new SerializableMessageCodec<Apply>("ApplyCodec"));
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
		return (Vertex) synchronize("getVertex", id);
	}

	@Override
	public Long[] getDependencies(long ts, long id) {
		return (Long[]) synchronize("getDependencies", new JsonObject().put("id", id).put("ts", ts));
	}

	@SuppressWarnings("unchecked")
	public <T> T synchronize(String methodName, Object parameters) {
		final CountDownLatch countDownLatch = new CountDownLatch(1);
		AsyncResult<?>[] replyArray = new AsyncResult<?>[1];
		bus.send(methodName, parameters, /* new DeliveryOptions().setSendTimeout(1000) */reply -> {
			replyArray[0] = reply;
			countDownLatch.countDown();
		});
		try {
			countDownLatch.await(5000, TimeUnit.MILLISECONDS);
		} catch (InterruptedException e) {
			engine.getCurrentCache().discardWithException(e);
		}
		if (replyArray[0] == null)
			engine.getCurrentCache().discardWithException(new IllegalStateException("no reply"));
		if (replyArray[0].succeeded())
			return (T) ((Message<Object>) replyArray[0].result()).body();
		engine.getCurrentCache().discardWithException(replyArray[0].cause());
		return null;// Unreachable
	}

	public static class Apply implements Serializable {

		private static final long serialVersionUID = 8725793789149242073L;
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
		synchronize("apply", new Apply(ts, removes, adds));
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
