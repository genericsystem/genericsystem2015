package org.genericsystem.distributed;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.AbstractEngine;
import org.genericsystem.common.HeavyCache;
import org.genericsystem.common.Generic;
import org.genericsystem.common.HeavyCache.ContextEventListener;
import org.genericsystem.common.Protocole.ClientCacheProtocole;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Statics;

public class HeavyClientEngine extends AbstractEngine implements Generic {

	protected final ClientCacheProtocole server;

	public HeavyClientEngine(Class<?>... userClasses) {
		this(Statics.ENGINE_VALUE, null, Statics.DEFAULT_PORT, userClasses);
	}

	public HeavyClientEngine(String engineValue, Class<?>... userClasses) {
		this(engineValue, null, Statics.DEFAULT_PORT, userClasses);
	}

	public HeavyClientEngine(String engineValue, String host, int port, Class<?>... userClasses) {
		this(engineValue, host, port, null, userClasses);
	}

	public HeavyClientEngine(String engineValue, String host, int port, String persistentDirectoryPath, Class<?>... userClasses) {
		init(this, buildHandler(getClass(), (Generic) this, Collections.emptyList(), engineValue, Collections.emptyList(), ApiStatics.TS_SYSTEM, ApiStatics.TS_SYSTEM));
		server = new WebSocketGSHeavyClient(host, port, "/" + engineValue);
		startSystemCache(userClasses);
		isInitialized = true;
	}

	@Override
	public HeavyClientEngine getRoot() {
		return this;
	}

	@Override
	public HeavyCache newCache() {
		return new HeavyCache(this) {
			@Override
			protected HeavyClientTransaction buildTransaction() {
				return new HeavyClientTransaction((HeavyClientEngine) (getRoot()), getRoot().pickNewTs());
			}
		};
	}

	public HeavyCache newCache(ContextEventListener<Generic> listener) {
		return new HeavyCache(this, listener) {
			@Override
			protected HeavyClientTransaction buildTransaction() {
				return new HeavyClientTransaction((HeavyClientEngine) (getRoot()), getRoot().pickNewTs());
			}
		};
	}

	@Override
	public HeavyCache getCurrentCache() {
		return (HeavyCache) super.getCurrentCache();
	}

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
		protected HeavyClientEngine getRoot() {
			return HeavyClientEngine.this;
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
