package org.genericsystem.cache;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Cache;
import org.genericsystem.common.ClientCacheProtocole;
import org.genericsystem.common.Cache.ContextEventListener;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Statics;

public class ClientEngine extends AbstractRoot implements Generic {

	protected final ClientCacheProtocole server;

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
		init(this, buildHandler(getClass(), (Generic) this, Collections.emptyList(), engineValue, Collections.emptyList(), ApiStatics.TS_SYSTEM, ApiStatics.TS_SYSTEM));
		server = new WebSocketGSClient(host, port, "/" + engineValue);
		startSystemCache(userClasses);
		isInitialized = true;
	}

	@Override
	public ClientEngine getRoot() {
		return this;
	}

	@Override
	public Cache newCache() {
		return new Cache(this) {
			@Override
			protected ClientTransaction buildTransaction() {
				return new ClientTransaction((ClientEngine) (getRoot()), getRoot().pickNewTs());
			}
		};
	}

	public Cache newCache(ContextEventListener<Generic> listener) {
		return new Cache(this, listener) {
			@Override
			protected ClientTransaction buildTransaction() {
				return new ClientTransaction((ClientEngine) (getRoot()), getRoot().pickNewTs());
			}
		};
	}

	// @Override
	// public Cache getCurrentCache() {
	// return super.getCurrentCache();
	// }

	// public static class LocalContextWrapper implements Wrapper {
	// private ThreadLocal<Cache> local = new InheritableThreadLocal<>();
	//
	// @Override
	// public void set(Cache context) {
	// if (context != null)
	// local.set(context);
	// else
	// local.remove();
	// }
	//
	// @Override
	// public Cache get() {
	// return local.get();
	// }
	// }

	// @Override
	// protected Wrapper buildContextWrapper() {
	// return new LocalContextWrapper();
	// }

	public Generic getGenericByVertex(Vertex vertex) {
		Generic generic = super.getGenericById(vertex.getTs());
		if (generic == null) {
			generic = build(vertex);
		}
		return generic;
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
	protected void finalize() throws Throwable {
		// System.out.println("FINALIZE CLIENT ENGINE !!!!!!!!");
		server.close();
		super.finalize();
	}

	@Override
	public void close() {
		server.close();
		super.close();
	}

	@Override
	protected Class<Generic> getTClass() {
		return Generic.class;
	}

	public ClientCacheProtocole getServer() {
		return server;
	}

	@Override
	protected Generic build(Long ts, Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long birthTs) {
		return init(newT(adaptClass(clazz, meta)), buildHandler(clazz, meta, supers, value, components, ts == null ? pickNewTs() : ts, birthTs));
	}

	ClientEngineHandler buildHandler(Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long ts, long birthTs) {
		return new ClientEngineHandler(clazz, meta, supers, value, components, ts, birthTs);
	}

	class ClientEngineHandler extends DefaultHandler {

		long birthTs;

		public ClientEngineHandler(Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long ts, long birthTs) {
			super(clazz, meta, supers, value, components, ts);
			this.birthTs = birthTs;
		}

		@Override
		protected ClientEngine getRoot() {
			return ClientEngine.this;
		}

		@Override
		public long getBirthTs() {
			return birthTs;
		}
	}

	@Override
	public long pickNewTs() {
		return server.pickNewTs();
	}

}