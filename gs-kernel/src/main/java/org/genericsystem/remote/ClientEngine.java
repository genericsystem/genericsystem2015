package org.genericsystem.remote;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.AbstractCache.ContextEventListener;
import org.genericsystem.common.Root;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Statics;
import org.genericsystem.common.SystemCache;
import org.genericsystem.common.Vertex;

/**
 * @author Nicolas Feybesse
 *
 */
public class ClientEngine extends Root implements Generic {

	protected final FrontEnd server;

	public ClientEngine(Class<?>... userClasses) {
		this(Statics.DEFAULT_HOST, Statics.ENGINES_DEFAULT_PORT, userClasses);
	}

	public ClientEngine(String path, Class<?>... userClasses) {
		this(Statics.DEFAULT_HOST, Statics.ENGINES_DEFAULT_PORT, path, userClasses);
	}

	public ClientEngine(String host, int port, Class<?>... userClasses) {
		this(host, port, "/", userClasses);
	}

	public ClientEngine(String host, int port, String path, Class<?>... userClasses) {
		assert path != null;
		init(this, buildHandler(getClass(), this, Collections.emptyList(), Statics.ENGINE_VALUE, Collections.emptyList(), ApiStatics.TS_SYSTEM,
				ApiStatics.TS_SYSTEM));
		server = new FrontEnd(host, port, path);
		startSystemCache(userClasses);
		isInitialized = true;
	}

	@Override
	public ClientEngine getRoot() {
		return this;
	}

	@Override
	protected SystemCache buildSystemCache(Root root) {
		return new SystemCache(root) {

			@Override
			protected Generic getOrBuild(AbstractCache cache, Class<?> clazz, Generic meta, List<Generic> overrides, Serializable value,
					List<Generic> components) {
				// TODO Auto-generated method stub
				Generic systemProperty = cache.get(meta, overrides, value, components);
				if (systemProperty == null)
					throw new IllegalStateException("Unable to find class on server : " + clazz.getName());
				return systemProperty;
			}

		};
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

	Generic getGenericByVertex(Vertex vertex) {
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

	public FrontEnd getServer() {
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
