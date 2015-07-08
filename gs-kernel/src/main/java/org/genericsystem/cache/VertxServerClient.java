package org.genericsystem.cache;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.EventBus;
import io.vertx.example.util.ExampleRunner;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Root;
import org.genericsystem.kernel.Server;

public class VertxServerClient extends AbstractVerticle implements Server {
	private final Server root = new Root();

	public static void main(String[] args) {
		ExampleRunner.runJavaExample("gs-kernel/src/main/java/", VertxServerClient.class, true);
	}

	@Override
	public Vertex getVertex(long id) {
		return root.getVertex(id);
	}

	@Override
	public long[] getDependencies(long ts, long id) {
		return root.getDependencies(ts, id);
	}

	@Override
	public void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		root.apply(ts, removes, adds);
	}

	@Override
	public long pickNewTs() {
		System.out.println("pick New Ts on root");
		return root.pickNewTs();
	}

	@Override
	public void close() {
		root.close();
	}

	@Override
	public void start() {
		EventBus eb = vertx.eventBus();
		System.out.println("aaa");

		// eb.consumer("getVertex", message -> {
		// System.out.println("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
		//
		// long id = (Long) message.body();
		// message.reply(getVertex(id));
		// });
		// System.out.println("bbb");
		//
		// eb.consumer("getDependencies", message -> {
		// System.out.println("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
		//
		// JsonObject temporal = (JsonObject) message.body();
		// System.out.println("yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy");
		// message.reply(getDependencies(temporal.getLong("ts"), temporal.getLong("id")));
		// System.out.println("zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz");
		// });
		eb.consumer("picknewts", message -> {
			System.out.println("Receive picknewts");

			message.reply(pickNewTs());
		});
		eb.consumer("ping-address", message -> {

			System.out.println("Received message: " + message.body());
			// Now send back reply
				message.reply("pong!");
			});
		// eb.consumer("close", message -> {
		// close();
		// message.reply(null);
		// });
		// eb.consumer("apply", message -> {
		// Apply apply = (Apply) message.body();
		// try {
		// apply(apply.ts, apply.removes, apply.adds);
		// } catch (Exception e) {
		// message.fail(500, e.getCause().getMessage());
		// }
		// message.reply(null);
		// });
		System.out.println("Receiver ready!");
	}
}
