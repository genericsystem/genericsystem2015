package org.genericsystem.cache;

import java.io.Serializable;
import java.util.List;

import org.genericsystem.common.AbstractCache.ContextEventListener;
import org.genericsystem.common.AbstractContext;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Root;
import org.genericsystem.kernel.Server;
import org.genericsystem.kernel.Statics;

public class ClientEngine extends AbstractRoot<ClientGeneric> implements ClientGeneric {

	private Server server;

	public ClientEngine(Class<?>... userClasses) {
		this(Statics.ENGINE_VALUE, userClasses);
	}

	public ClientEngine(Serializable engineValue, Class<?>... userClasses) {
		this(engineValue, null, userClasses);
	}

	public ClientEngine(Serializable engineValue, String persistentDirectoryPath, Class<?>... userClasses) {
		super(engineValue, persistentDirectoryPath, userClasses);
		isInitialized = true;
	}

	@Override
	public ClientEngine getRoot() {
		return this;
	}

	@Override
	protected void initSubRoot(Serializable engineValue, String persistentDirectoryPath, Class<?>... userClasses) {
		server = new ClientServer(new Root(engineValue, persistentDirectoryPath, userClasses));
		// server = new VertxClientServer(this, vertx);
	};

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
}