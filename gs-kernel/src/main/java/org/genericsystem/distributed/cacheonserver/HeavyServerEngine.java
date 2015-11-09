package org.genericsystem.distributed.cacheonserver;

import java.io.Serializable;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.NotFoundException;
import org.genericsystem.common.Generic;
import org.genericsystem.common.HeavyCache;
import org.genericsystem.common.HeavyCache.ContextEventListener;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.EngineImpl;
import org.genericsystem.kernel.Statics;

public class HeavyServerEngine extends EngineImpl implements ServerCacheProtocole {

	// private ThreadLocal<Long> contextIds = new ThreadLocal<>();
	// private ConcurrentHashMap<Long, AbstractCache> map;

	public HeavyServerEngine(Class<?>... userClasses) {
		this(Statics.ENGINE_VALUE, userClasses);
	}

	public HeavyServerEngine(String engineValue, Class<?>... userClasses) {
		this(engineValue, null, userClasses);
	}

	public HeavyServerEngine(String engineValue, String persistentDirectoryPath, Class<?>... userClasses) {
		super(engineValue, persistentDirectoryPath, userClasses);
	}

	@Override
	protected Generic init(Generic generic, DefaultHandler handler) {
		return super.init(generic, handler);
	}

	@Override
	public HeavyServerEngine getRoot() {
		return this;
	}

	@Override
	public HeavyCache newCache() {
		// HeavyCache cache = new HeavyCache(this) {
		// @Override
		// protected IDifferential<Generic> buildTransaction() {
		// return new Transaction((AbstractServer) getRoot());
		// }
		// };
		// HeavyCache result = (HeavyCache) map.putIfAbsent(cache.getCacheId(), cache);
		// assert result == null;
		return super.newCache();
	}

	@Override
	public HeavyCache newCache(ContextEventListener<Generic> listener) {
		// HeavyCache cache = new HeavyCache(this, listener) {
		// @Override
		// protected IDifferential<Generic> buildTransaction() {
		// return new Transaction((AbstractServer) getRoot());
		// }
		// };
		// HeavyCache result = (HeavyCache) map.putIfAbsent(cache.getCacheId(), cache);
		// assert result == null;
		return super.newCache(listener);
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

	// @Override
	// public long flush(long cacheId) {
	// return this.<Long> safeContextExecute(cacheId, cache -> {
	// long ts = getCurrentCache().getTs();
	// getCurrentCache().flush();
	// long newTs = getCurrentCache().getTs();
	// return newTs == ts ? ts : Statics.CONCURRENCY_CONTROL_EXCEPTION;
	// });
	// }

	@Override
	public long tryFlush(long cacheId) throws ConcurrencyControlException {
		return this.<Long> concurrentSafeContextExecute(cacheId, cache -> {
			getCurrentCache().tryFlush();
			return getCurrentCache().getTs();
		});
	}
}
