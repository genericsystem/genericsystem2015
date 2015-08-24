package org.genericsystem.cache;

import io.vertx.core.Vertx;

import java.io.Serializable;
import java.util.List;

import org.genericsystem.common.AbstractCache.ContextEventListener;
import org.genericsystem.common.AbstractContext;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Server;
import org.genericsystem.kernel.Statics;

public class ClientEngine extends AbstractRoot<ClientGeneric> implements ClientGeneric {

	protected Server server;

	public ClientEngine(Vertx vertx, Class<?>... userClasses) {
		this(vertx, Statics.ENGINE_VALUE, null, Statics.DEFAULT_PORT, userClasses);
	}

	public ClientEngine(Vertx vertx, String engineValue, Class<?>... userClasses) {
		this(vertx, engineValue, null, Statics.DEFAULT_PORT, userClasses);
	}

	public ClientEngine(Vertx vertx, String engineValue, String host, int port, Class<?>... userClasses) {
		this(vertx, engineValue, host, port, null, userClasses);
	}

	public ClientEngine(Vertx vertx, String engineValue, String host, int port, String persistentDirectoryPath, Class<?>... userClasses) {
		super(vertx, engineValue, host, port, persistentDirectoryPath, userClasses);
		isInitialized = true;
	}

	@Override
	public ClientEngine getRoot() {
		return this;
	}

	@Override
	protected void initSubRoot(Vertx vertx, String engineValue, String host, int port, String persistentDirectoryPath, Class<?>... userClasses) {
		server = new HttpGSClient(vertx, this, host, port, "/" + engineValue);
	}

	@Override
	public ClientCache newCache() {
		return new ClientCache(this);
	}

	public ClientCache newCache(ContextEventListener<ClientGeneric> listener) {
		return new ClientCache(this, listener);
	}

	@Override
	public ClientCache getCurrentCache() {
		return (ClientCache) super.getCurrentCache();
	}

	public static class LocalContextWrapper extends InheritableThreadLocal<ClientCache> implements Wrapper<ClientGeneric> {
		@Override
		public void set(AbstractContext<ClientGeneric> context) {
			super.set((ClientCache) context);
		}
	}

	@Override
	protected Wrapper<ClientGeneric> buildContextWrapper() {
		return new LocalContextWrapper();
	}

	@Override
	public ClientGeneric getGenericById(long ts) {
		ClientGeneric generic = super.getGenericById(ts);
		if (generic == null) {
			Vertex vertex = server.getVertex(ts);
			generic = build(vertex);
		}
		return generic;
	}

	@Override
	public final ClientGeneric[] newTArray(int dim) {
		return new ClientGeneric[dim];
	}

	@Override
	public void close() {
		server.close();
	}

	@Override
	protected Class<ClientGeneric> getTClass() {
		return ClientGeneric.class;
	}

	public Server getServer() {
		return server;
	}

	@Override
	protected EngineWrapped buildHandler(Class<?> clazz, ClientGeneric meta, List<ClientGeneric> supers, Serializable value, List<ClientGeneric> components, long ts, long[] otherTs) {
		return new EngineWrapped(clazz, meta, supers, value, components, ts, otherTs);
	}

	class EngineWrapped extends Wrapped {

		public EngineWrapped(Class<?> clazz, ClientGeneric meta, List<ClientGeneric> supers, Serializable value, List<ClientGeneric> components, long ts, long[] otherTs) {
			super(clazz, meta, supers, value, components, ts, otherTs);
		}

		@Override
		protected ClientEngine getRoot() {
			return ClientEngine.this;
		}
	}

	@Override
	public long pickNewTs() {
		return server.pickNewTs();
	}

	// public static void main(String[] args) {
	// ExampleRunner.runJavaExample("gs-kernel/src/main/java/",
	// ClientEngine.class, true);
	// }
}