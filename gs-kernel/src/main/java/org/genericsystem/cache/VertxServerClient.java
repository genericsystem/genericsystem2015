package org.genericsystem.cache;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.EventBus;
import io.vertx.core.json.JsonObject;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.cache.VertxClientServer.Apply;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Root;
import org.genericsystem.kernel.Server;

public class VertxServerClient extends AbstractVerticle implements Server {
	private final Server root = new Root();

	@Override
	public Vertex getVertex(long id) {
		return root.getVertex(id);
	}

	@Override
	public Long[] getDependencies(long ts, long id) {
		return root.getDependencies(ts, id);
	}

	@Override
	public void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		root.apply(ts, removes, adds);
	}

	@Override
	public long pickNewTs() {
		return root.pickNewTs();
	}

	@Override
	public void close() {
		root.close();
	}

	@Override
	public void start() {
		EventBus eb = vertx.eventBus();
		eb.consumer("pickNewTs", message -> {
			try {
				// System.out.println("Receive picknewts");
				message.reply(pickNewTs());
			} catch (Throwable e) {
				e.printStackTrace();
				message.fail(400, e.getMessage());
			}
		});
		eb.consumer("getDependencies", message -> {
			try {
				// System.out.println("Receive getDependencies");
				JsonObject json = (JsonObject) message.body();
				message.reply(getDependencies(json.getLong("ts"), json.getLong("id")));
				// message.reply(new JsonArray(Arrays.asList(getDependencies(json.getLong("ts"), json.getLong("id")))));
			} catch (Throwable e) {
				e.printStackTrace();
				message.fail(400, e.getMessage());
			}
		});
		eb.consumer("getVertex", message -> {
			try {
				// System.out.println("Receive getVertex");
				long id = (Long) message.body();
				message.reply(getVertex(id));
			} catch (Throwable e) {
				e.printStackTrace();
				message.fail(400, e.getMessage());
			}
		});
		eb.consumer("apply", message -> {
			try {
				// System.out.println("Receive apply");
				Apply apply = (Apply) message.body();
				apply(apply.ts, apply.removes, apply.adds);
				message.reply(null);
			} catch (Throwable e) {
				e.printStackTrace();
				message.fail(400, e.getMessage());
			}
		});

		System.out.println("Receiver ready!");
	}
}
