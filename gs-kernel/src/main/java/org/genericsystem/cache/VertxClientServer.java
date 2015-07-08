package org.genericsystem.cache;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.json.JsonObject;
import io.vertx.example.util.ExampleRunner;
import java.util.concurrent.CountDownLatch;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Server;

public class VertxClientServer extends AbstractVerticle implements Server {
	private ClientEngine engine;

	@Override
	public void start() throws Exception {
		// vertx.setPeriodic(1000, v -> {
		//
		// vertx.eventBus().send("ping-address", "ping!", reply -> {
		// if (reply.succeeded()) {
		// System.out.println("Received reply " + reply.result().body());
		// } else {
		// System.out.println("No reply");
		// }
		// });
		//
		// });
		// vertx.setTimer(5000, future -> {
		// vertx.eventBus().send("picknewts", null, reply -> {
		// if (reply.succeeded()) {
		// System.out.println("Received reply " + reply.result().body());
		// } else {
		// System.out.println("No reply");
		// }
		// });
		// });
		long[] resultHandler = new long[1];
		final CountDownLatch countDownLatch = new CountDownLatch(1);
		vertx.executeBlocking(future -> {
			vertx.eventBus().send("picknewts", null, reply -> {
				if (reply.succeeded()) {
					resultHandler[0] = (long) reply.result().body();
					countDownLatch.countDown();
					System.out.println("all is ok picknewts");
				} else {
					System.out.println("ko  picknewts");
					engine.getCurrentCache().discardWithException(reply.cause());
				}
			});
			try {
				countDownLatch.await();
			} catch (InterruptedException e) {
				engine.getCurrentCache().discardWithException(e);
			}
			future.complete(resultHandler[0]);
		}, result -> {
			if (result.succeeded())
				System.out.println("successsssssssssssss");
			else
				System.out.println("faillllllllllllllllll");

		});
		try {
			countDownLatch.await();
		} catch (InterruptedException e) {
			engine.getCurrentCache().discardWithException(e);
		}
		System.out.println("pickNewTs ok : " + resultHandler[0]);
		// this.engine = new ClientEngine() {
		// @Override
		// protected void initSubRoot(Serializable engineValue, String persistentDirectoryPath, Class<?>... userClasses) {
		// server = VertxClientServer.this;
		// }
		// };
	}

	public static void main(String[] args) {
		ExampleRunner.runJavaExample("gs-kernel/src/main/java/", VertxClientServer.class, true);
	}

	@Override
	public Vertex getVertex(long id) {
		System.out.println("getVertex");
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
		JsonObject json = new JsonObject();
		json.put("id", id);
		json.put("ts", ts);
		System.out.println("fffffffffffffffffffffffffff");
		vertx.eventBus().send("getDependencies", json, reply -> {
			System.out.println("rrrrrrrrrrrrrrrrrrrrrr");
			if (reply.succeeded()) {
				System.out.println("sssssssssssssssssssssssssssss");
				resultHandler[0] = (long[]) reply.result().body();
				countDownLatch.countDown();
			} else {
				System.out.println("tttttttttttttttttttttttttttt");
				engine.getCurrentCache().discardWithException(reply.cause());
				throw new IllegalStateException(reply.cause());
			}
		});
		try {
			System.out.println("uuuuuuuuuuuuuuuuuuuuuuu");
			countDownLatch.await();
		} catch (InterruptedException e) {
			engine.getCurrentCache().discardWithException(e);
		}
		return resultHandler[0];
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
		vertx.eventBus().send("apply", new Apply(ts, removes, adds), reply -> {
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
		System.out.println("pickNewTs");
		long[] resultHandler = new long[1];
		final CountDownLatch countDownLatch = new CountDownLatch(1);
		vertx.eventBus().send("picknewts", null, reply -> {
			if (reply.succeeded()) {
				resultHandler[0] = (long) reply.result().body();
				countDownLatch.countDown();
				System.out.println("all is ok picknewts");
			} else {
				System.out.println("ko  picknewts");
				engine.getCurrentCache().discardWithException(reply.cause());
			}
		});
		try {
			countDownLatch.await();
		} catch (InterruptedException e) {
			engine.getCurrentCache().discardWithException(e);
		}
		System.out.println("pickNewTs ok : " + resultHandler[0]);
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
