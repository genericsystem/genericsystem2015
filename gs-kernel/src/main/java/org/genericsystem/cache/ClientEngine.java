package org.genericsystem.cache;

import java.io.Serializable;
import java.util.List;

import org.genericsystem.common.AbstractCache.ContextEventListener;
import org.genericsystem.common.AbstractContext;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Generic;
import org.genericsystem.kernel.Server;
import org.genericsystem.kernel.Statics;

public class ClientEngine extends AbstractRoot<Generic> implements Generic {

	protected Server server;

	public ClientEngine(Class<?>... userClasses) {
		this(Statics.ENGINE_VALUE, null, Statics.DEFAULT_PORT, userClasses);
	}

	public ClientEngine(String engineValue, Class<?>... userClasses) {
		this(engineValue, null, Statics.DEFAULT_PORT, userClasses);
	}

	public ClientEngine(String engineValue, String host, int port, Class<?>... userClasses) {
		this(engineValue, host, port, null, userClasses);
	}

	public ClientEngine(String engineValue, String host, int port, String persistentDirectoryPath, Class<?>... userClasses) {
		super(engineValue, host, port, persistentDirectoryPath, userClasses);
		isInitialized = true;
	}

	@Override
	public ClientEngine getRoot() {
		return this;
	}

	@Override
	protected void initSubRoot(String engineValue, String host, int port, String persistentDirectoryPath, Class<?>... userClasses) {
		server = new HttpGSClient(this, host, port, "/" + engineValue);
	}

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
			generic = build(vertex);
		}
		return generic;
	}

	@Override
	public final Generic[] newTArray(int dim) {
		return new Generic[dim];
	}

	@Override
	public void close() {
		server.close();
	}

	@Override
	protected Class<Generic> getTClass() {
		return Generic.class;
	}

	public Server getServer() {
		return server;
	}

	@Override
	protected EngineWrapped buildHandler(Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long ts, long[] otherTs) {
		return new EngineWrapped(clazz, meta, supers, value, components, ts, otherTs);
	}

	class EngineWrapped extends DefaultHandler {

		public EngineWrapped(Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long ts, long[] otherTs) {
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