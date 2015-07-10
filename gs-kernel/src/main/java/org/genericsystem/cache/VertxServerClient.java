package org.genericsystem.cache;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.EventBus;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.example.util.ExampleRunner;
import java.util.Arrays;
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
	public Long[] getDependencies(long ts, long id) {
		System.out.println("getDependencies : " + ts + " " + id);
		System.out.println("getDependencies : " + Arrays.asList(root.getDependencies(ts, id)));
		System.out.println("Vertex found " + getVertex(root.getDependencies(ts, id)[0]).getTs());
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
		eb.consumer("pickNewTs", message -> {
			System.out.println("Receive picknewts");
			message.reply(pickNewTs());
		});
		eb.consumer("getDependencies", message -> {
			System.out.println("Receive getDependencies");
			JsonObject json = (JsonObject) message.body();
			message.reply(new JsonArray(Arrays.asList(getDependencies(json.getLong("ts"), json.getLong("id")))));
		});
		eb.consumer("getVertex", message -> {
			System.out.println("Receive getVertex");
			long id = (Long) message.body();
			System.out.println("id : " + id);
			Vertex vertex = getVertex(id);
			JsonObject json = vertex.getJsonObject();
			message.reply(json);
		});

		System.out.println("Receiver ready!");
	}
}
