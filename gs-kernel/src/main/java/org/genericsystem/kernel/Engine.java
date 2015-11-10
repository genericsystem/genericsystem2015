package org.genericsystem.kernel;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.NotFoundException;
import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.Generic;
import org.genericsystem.common.HeavyCache;
import org.genericsystem.common.HeavyCache.ContextEventListener;
import org.genericsystem.common.IDifferential;
import org.genericsystem.common.Vertex;
import org.genericsystem.defaults.DefaultCache;
import org.genericsystem.distributed.cacheonserver.CosProtocole;

public class Engine extends AbstractServer implements CosProtocole {

	private ThreadLocal<Long> contextIds = new ThreadLocal<>();
	private ConcurrentHashMap<Long, AbstractCache> map;

	public Engine(Class<?>... userClasses) {
		this(Statics.ENGINE_VALUE, userClasses);
	}

	public Engine(String engineValue, Class<?>... userClasses) {
		this(engineValue, null, userClasses);
	}

	public Engine(String engineValue, String persistentDirectoryPath, Class<?>... userClasses) {
		init(this, buildHandler(getClass(), (Generic) this, Collections.emptyList(), engineValue, Collections.emptyList(), ApiStatics.TS_SYSTEM, ApiStatics.SYSTEM_TS));
		map = new ConcurrentHashMap<>();
		startSystemCache(userClasses);
		archiver = new Archiver(this, persistentDirectoryPath);
		isInitialized = true;
		newCache().start();
	}

	@Override
	protected Generic init(Generic generic, DefaultHandler handler) {
		return super.init(generic, handler);
	}

	@Override
	public Engine getRoot() {
		return this;
	}

	@Override
	public HeavyCache newCache() {
		HeavyCache cache = new HeavyCache(this) {
			@Override
			protected IDifferential<Generic> buildTransaction() {
				return new Transaction((AbstractServer) getRoot());
			}
		};
		HeavyCache result = (HeavyCache) map.putIfAbsent(cache.getCacheId(), cache);
		assert result == null;
		return cache;
	}

	public HeavyCache newCache(ContextEventListener<Generic> listener) {
		HeavyCache cache = new HeavyCache(this, listener) {
			@Override
			protected IDifferential<Generic> buildTransaction() {
				return new Transaction((AbstractServer) getRoot());
			}
		};
		HeavyCache result = (HeavyCache) map.putIfAbsent(cache.getCacheId(), cache);
		assert result == null;
		return cache;
	}

	@Override
	public long newCacheId() {
		HeavyCache cache = newCache();
		return cache.getCacheId();
	}

	@Override
	public int getCacheLevel(long cacheId) {
		return safeContextExecute(cacheId, cache -> getCurrentCache().getCacheLevel());

	}

	@Override
	public long mount(long cacheId) {
		return safeContextExecute(cacheId, cache -> {
			getCurrentCache().mount();// TODO must return getTs() ?
				return getTs();
			});
	}

	@Override
	public long unmount(long cacheId) {
		return safeContextExecute(cacheId, cache -> {
			getCurrentCache().unmount();// TODO must return getTs() ?
				return getTs();
			});
	}

	@Override
	public long clear(long cacheId) {

		return safeContextExecute(cacheId, cache -> {
			getCurrentCache().clear();// TODO must return getTs() ?
				return getTs();
			});
	}

	@Override
	public long shiftTs(long cacheId) {
		return safeContextExecute(cacheId, cache -> getCurrentCache().shiftTs());
	}

	@Override
	public Vertex[] getDependencies(long cacheId, long id) {
		Generic ancestor = this.getGenericById(id);
		return ancestor != null ? safeContextExecute(cacheId, cache -> getCurrentCache().getDependencies(ancestor).stream().map(generic -> generic.getVertex()).toArray(Vertex[]::new)) : Statics.EMPTY;
	}

	@Override
	public long addInstance(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return safeContextExecute(cacheId,
				cache -> getCurrentCache().addInstance(getById(meta), overrides.stream().map(override -> getById(override)).collect(Collectors.toList()), value, components.stream().map(component -> getById(component)).collect(Collectors.toList())).getTs());

	}

	private Generic getById(long genericId) {
		Generic generic = getRoot().getGenericById(genericId);
		if (generic == null)
			getCurrentCache().discardWithException(new NotFoundException("Unknown Id : " + genericId));
		return generic;
	}

	@Override
	public long update(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> components) {
		return safeContextExecute(cacheId,
				cache -> getCurrentCache().update(getById(update), overrides.stream().map(override -> getById(override)).collect(Collectors.toList()), value, components.stream().map(component -> getById(component)).collect(Collectors.toList())).getTs());
	}

	@Override
	public long merge(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> components) {
		return safeContextExecute(cacheId,
				cache -> getCurrentCache().merge(getById(update), overrides.stream().map(override -> getById(override)).collect(Collectors.toList()), value, components.stream().map(component -> getById(component)).collect(Collectors.toList())).getTs());
	}

	@Override
	public long setInstance(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return safeContextExecute(cacheId,
				cache -> getCurrentCache().setInstance(getById(meta), overrides.stream().map(override -> getById(override)).collect(Collectors.toList()), value, components.stream().map(component -> getById(component)).collect(Collectors.toList())).getTs());
	}

	static interface ConcurrenctFunction<T, R> {
		R apply(T t) throws ConcurrencyControlException;
	}

	private <T> T safeContextExecute(long cacheId, Function<HeavyCache, T> function) {
		// System.out.println("Safe context : " + cacheId);
		HeavyCache cache = getCurrentCache(cacheId);
		cache.start();
		try {
			return function.apply(cache);
		} finally {
			cache.stop();
		}
	}

	private <T> T concurrentSafeContextExecute(long cacheId, ConcurrenctFunction<HeavyCache, T> function) throws ConcurrencyControlException {
		// System.out.println("Safe context : " + cacheId);
		HeavyCache cache = getCurrentCache(cacheId);
		cache.start();
		try {
			return function.apply(cache);
		} finally {
			cache.stop();
		}
	}

	@Override
	public long remove(long cacheId, long generic) {
		return this.<Long> safeContextExecute(cacheId, cache -> {
			Generic toRemove = getById(generic);
			getCurrentCache().remove(toRemove);
			return toRemove.getTs();
		});

	}

	@Override
	public long forceRemove(long cacheId, long generic) {
		return this.<Long> safeContextExecute(cacheId, cache -> {
			Generic toRemove = getById(generic);
			getCurrentCache().forceRemove(toRemove);
			return toRemove.getTs();
		});
	}

	@Override
	public long conserveRemove(long cacheId, long generic) {
		return this.<Long> safeContextExecute(cacheId, cache -> {
			Generic toRemove = getById(generic);
			getCurrentCache().conserveRemove(toRemove);
			return toRemove.getTs();
		});
	}

	@Override
	public long flush(long cacheId) {
		return this.<Long> safeContextExecute(cacheId, cache -> {
			long ts = getCurrentCache().getTs();
			getCurrentCache().flush();
			long newTs = getCurrentCache().getTs();
			return newTs == ts ? ts : Statics.CONCURRENCY_CONTROL_EXCEPTION;
		});
	}

	@Override
	public long tryFlush(long cacheId) throws ConcurrencyControlException {
		return this.<Long> concurrentSafeContextExecute(cacheId, cache -> {
			getCurrentCache().tryFlush();
			return getCurrentCache().getTs();
		});
	}

	@Override
	public HeavyCache getCurrentCache() {
		if (getRoot().isInitialized())
			assert contextIds.get() != null : contextIds.get();
		return (HeavyCache) (getRoot().isInitialized() ? getCurrentCache(contextIds.get()) : super.getCurrentCache());
	}

	@Override
	protected AbstractCache start(AbstractCache context) {
		if (!isInitialized()) {
			// System.out.println("system context is ok");
			super.start(context);
			return context;
		}
		long cacheId = ((HeavyCache) context).getCacheId();
		// map.put(cacheId, context);
		contextIds.set(cacheId);
		assert getCurrentCache() == context;
		// System.out.println("context ok cacheId : " + cacheId + " ts : " + ((Cache) context).getTs());
		return context;
	}

	@Override
	protected void stop(DefaultCache<Generic> context) {
		if (!isInitialized()) {
			super.stop(context);
			return;
		}
		// Long cacheId = ((Cache) context).getCacheId();
		contextIds.remove();
		// map.remove(cacheId);
	}

	public HeavyCache getCurrentCache(long cacheId) {
		return (HeavyCache) map.get(cacheId);
		// Cache cache = (Cache) map.get(cacheId);
		// if (cache != null)
		// return cache;
		// Cache result = (Cache) map.putIfAbsent(cacheId, cache = newCache(cacheId));
		// return result != null ? result : cache;
	}

	// private Cache newCache(long cacheId) {
	// return new Cache(this, cacheId) {
	// @Override
	// protected IDifferential<Generic> buildTransaction() {
	// return new Transaction((AbstractRoot) getRoot());
	// }
	// };
	// }

	@Override
	protected void finalize() throws Throwable {
		System.out.println("FINALIZE");
		super.finalize();
	}

}