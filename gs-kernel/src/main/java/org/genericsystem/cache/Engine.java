package org.genericsystem.cache;

import java.io.Serializable;

import org.genericsystem.cache.Cache.ContextEventListener;
import org.genericsystem.kernel.AbstractContext;
import org.genericsystem.kernel.AbstractRoot;
import org.genericsystem.kernel.Root;
import org.genericsystem.kernel.Statics;
import org.genericsystem.kernel.Vertex;

public class Engine extends AbstractRoot<Generic> implements Generic {

	private Root server;
	private boolean isInitialized = false;

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
	protected void initSubRoot(Serializable engineValue, String persistentDirectoryPath, Class<?>... userClasses) {
		server = new Root(engineValue, persistentDirectoryPath, userClasses);
	};

	@Override
	public Cache newCache() {
		return new Cache(this);
	}

	@Override
	protected void flushContext() {
		getCurrentCache().flush();
	}

	public Cache newCache(ContextEventListener<Generic> listener) {
		return new Cache(new Transaction(this), listener);
	}

	protected Cache start(Cache cache) {
		contextWrapper.set(cache);
		return cache;
	}

	protected void stop(Cache cache) {
		assert contextWrapper.get() == cache;
		contextWrapper.set(null);
	}

	@Override
	public Cache getCurrentCache() {
		Cache currentCache = (Cache) contextWrapper.get();
		if (currentCache == null)
			throw new IllegalStateException("Unable to find the current cache. Did you miss to call start() method on it ?");
		return currentCache;
	}

	public static class LocalContextWrapper extends InheritableThreadLocal<Cache> implements Wrapper<Generic> {
		@Override
		public void set(AbstractContext<Generic> context) {
			super.set((Cache) context);
		}
	}

	@Override
	protected Wrapper<Generic> buildContextWrapper() {
		return new LocalContextWrapper();
	}

	@Override
	public Generic getGenericFromTs(long ts) {
		Generic generic = idsMap.get(ts);
		if (generic == null) {
			Vertex vertex = server.getVertex(ts);
			if (vertex == null)
				return null;
			Class<?> clazz = server.getAnnotedClass(server.getGenericFromTs(ts));
			generic = init(newT(clazz, ts == vertex.getMeta() ? null : getGenericFromTs(vertex.getMeta())), ts, vertex.getMeta(), vertex.getSupers(), vertex.getValue(), vertex.getComponents(), vertex.getLifeManager());
			idsMap.put(ts, generic);
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

	@Override
	protected boolean isInitialized() {
		return isInitialized;
	}

	public Root getServer() {
		return server;
	}

}