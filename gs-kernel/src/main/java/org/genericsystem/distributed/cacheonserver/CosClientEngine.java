package org.genericsystem.distributed.cacheonserver;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Statics;

public class CosClientEngine extends AbstractRoot implements Generic {

	protected final CosProtocole server;

	public CosClientEngine(Class<?>... userClasses) {
		this(Statics.ENGINE_VALUE, null, Statics.DEFAULT_PORT, userClasses);
	}

	public CosClientEngine(String engineValue, Class<?>... userClasses) {
		this(engineValue, null, Statics.DEFAULT_PORT, userClasses);
	}

	public CosClientEngine(String engineValue, String host, int port, Class<?>... userClasses) {
		this(engineValue, host, port, null, userClasses);
	}

	public CosClientEngine(String engineValue, String host, int port, String persistentDirectoryPath, Class<?>... userClasses) {
		init(this, buildHandler(getClass(), (Generic) this, Collections.emptyList(), engineValue, Collections.emptyList(), ApiStatics.TS_SYSTEM, ApiStatics.TS_SYSTEM));
		server = new WebSocketCosClient(host, port, "/" + engineValue);
		startSystemCache(userClasses);
		isInitialized = true;
	}

	@Override
	public CosClientEngine getRoot() {
		return this;
	}

	@Override
	public CosCache newCache() {
		return new CosCache(this);
	}

	@Override
	public CosCache getCurrentCache() {
		// Cache context = contextWrapper.get();
		if (context == null)
			throw new IllegalStateException("Unable to find the current cache. Did you miss to call start() method on it ?");
		return ((CosCache) super.getCurrentCache());
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

	public CosProtocole getServer() {
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
		protected CosClientEngine getRoot() {
			return CosClientEngine.this;
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
