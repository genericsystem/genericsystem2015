package org.genericsystem.mutability;

import java.io.Serializable;
import java.lang.reflect.Method;
import java.util.List;

import javassist.util.proxy.MethodHandler;

import org.genericsystem.defaults.DefaultRoot;
import org.genericsystem.kernel.ServerEngine;
import org.genericsystem.kernel.Statics;

public class Engine implements Generic, DefaultRoot<Generic>, MethodHandler {

	protected final InheritableThreadLocal<Cache> cacheLocal = new InheritableThreadLocal<>();

	private final ServerEngine cacheEngine;

	public Engine(Class<?>... userClasses) {
		this(Statics.ENGINE_VALUE, userClasses);
	}

	public Engine(String engineValue, Class<?>... userClasses) {
		this(engineValue, null, userClasses);
	}

	public Engine(String engineValue, String persistentDirectoryPath, Class<?>... userClasses) {
		this.cacheEngine = new ServerEngine(engineValue, persistentDirectoryPath, userClasses);
		newCache().start();
	}

	@Override
	public boolean isSystem() {
		return cacheEngine.isSystem();
	}

	@Override
	public Object invoke(Object self, Method m, Method proceed, Object[] args) throws Throwable {
		if (m.getName().equals("getRoot"))
			return this;
		return ((Generic) self).defaultToString();
	}

	@Override
	public Engine getRoot() {
		return this;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <Custom extends Generic> Custom find(Class<?> clazz) {
		return (Custom) getCurrentCache().wrap(clazz, cacheEngine.find(clazz));
	}

	@SuppressWarnings("unchecked")
	@Override
	public <Custom extends Generic> Custom bind(Class<?> clazz) {
		return (Custom) getCurrentCache().wrap(clazz, cacheEngine.bind(clazz));
	}

	@Override
	public Class<?> findAnnotedClass(Generic vertex) {
		return cacheEngine.findAnnotedClass(getCurrentCache().unwrap(vertex));
	}

	@Override
	public Cache newCache() {
		return new Cache(this, cacheEngine);
	}

	Cache start(Cache cache) {
		if (!equals(cache.getRoot()))
			throw new IllegalStateException();
		cacheLocal.set(cache);
		return cache;
	}

	void stop(Cache cache) {
		assert cacheLocal.get() == cache;
		cacheLocal.set(null);
	}

	@Override
	public Cache getCurrentCache() {
		Cache currentCache = cacheLocal.get();
		if (currentCache == null)
			throw new IllegalStateException("Unable to find the current cache. Did you miss to call start() method on it ?");
		return currentCache;
	}

	public org.genericsystem.kernel.ServerEngine getConcurrencyEngine() {
		return cacheEngine;
	}

	@Override
	public void close() {
		cacheEngine.close();
	}

	public long getTs(Generic generic) {
		return getCurrentCache().unwrap(generic).getTs();
	}

	public Generic getMeta(Generic generic) {
		return getCurrentCache().wrap(getCurrentCache().unwrap(generic).getMeta());
	}

	public List<Generic> getSupers(Generic generic) {
		return getCurrentCache().wrap(getCurrentCache().unwrap(generic).getSupers());
	}

	public Serializable getValue(Generic generic) {
		return getCurrentCache().unwrap(generic).getValue();
	}

	public List<Generic> getComponents(Generic generic) {
		return getCurrentCache().wrap(getCurrentCache().unwrap(generic).getComponents());
	}

	@Override
	public String toString() {
		return defaultToString();
	}

	@Override
	public Generic[] newTArray(int i) {
		return new Generic[i];
	}
}
