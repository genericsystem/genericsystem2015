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

public class Engine extends AbstractRoot<Generic> implements Generic {

	private Root server;

	public Engine(Class<?>... userClasses) {
		this(Statics.ENGINE_VALUE, userClasses);
	}

	public Engine(Serializable engineValue, Class<?>... userClasses) {
		this(engineValue, null, userClasses);
	}

	public Engine(Serializable engineValue, String persistentDirectoryPath, Class<?>... userClasses) {
		super(engineValue, persistentDirectoryPath, userClasses);
		isInitialized = true;
	}

	@Override
	public Engine getRoot() {
		return this;
	}

	@Override
	protected void initSubRoot(Serializable engineValue, String persistentDirectoryPath, Class<?>... userClasses) {
		server = new Root(engineValue, persistentDirectoryPath, userClasses);
	};

	@Override
	public ClientCache newCache() {
		return new ClientCache(this);
	}

	public ClientCache newCache(ContextEventListener<Generic> listener) {
		return new ClientCache(this, listener);
	}

	@Override
	public ClientCache getCurrentCache() {
		return (ClientCache) super.getCurrentCache();
	}

	public static class LocalContextWrapper extends InheritableThreadLocal<ClientCache> implements Wrapper<Generic> {
		@Override
		public void set(AbstractContext<Generic> context) {
			super.set((ClientCache) context);
		}
	}

	@Override
	protected Wrapper<Generic> buildContextWrapper() {
		return new LocalContextWrapper();
	}

	@Override
	public Generic getGenericById(long ts) {
		Generic generic = super.getGenericById(ts);
		if (generic == null) {
			Vertex vertex = server.getVertex(ts);
			generic = init(server.getAnnotedClass(server.getGenericById(ts)), vertex);
		}
		return generic;
	}

	@Override
	public final Generic[] newTArray(int dim) {
		return new Generic[dim];
	}

	@Override
	public void close() {
		// server.close();
	}

	@Override
	protected Class<Generic> getTClass() {
		return Generic.class;
	}

	public Server getServer() {
		return server;
	}

	@Override
	protected EngineWrapped buildHandler(Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long ts, long[] otherTs) {
		return new EngineWrapped(meta, supers, value, components, ts, otherTs);
	}

	class EngineWrapped extends Wrapped {

		public EngineWrapped(Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long ts, long[] otherTs) {
			super(meta, supers, value, components, ts, otherTs);
		}

		@Override
		protected Engine getRoot() {
			return Engine.this;
		}
	}

}