package org.genericsystem.kernel;

import java.io.Serializable;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.Cache;
import org.genericsystem.common.Cache.ContextEventListener;
import org.genericsystem.common.Generic;
import org.genericsystem.common.IDifferential;
import org.genericsystem.common.Protocole.ServerCacheProtocole;
import org.genericsystem.common.Vertex;
import org.genericsystem.defaults.DefaultCache;

public class HeavyServerEngine extends AbstractRoot implements ServerCacheProtocole {

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
	public HeavyServerEngine getRoot() {
		return this;
	}

	@Override
	public Cache newCache() {
		return new Cache(this) {
			@Override
			protected IDifferential<Generic> buildTransaction() {
				return new Transaction((Root) getRoot());
			}
		};
	}

	public Cache newCache(ContextEventListener<Generic> listener) {
		return new Cache(this, listener) {
			@Override
			protected IDifferential<Generic> buildTransaction() {
				return new Transaction((Root) getRoot());
			}
		};
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
		// TODO rollback
	}

	@Override
	public Vertex[] getDependencies(long cacheId, long id) {
		Generic ancestor = this.getGenericById(id);
		return ancestor != null ? safeContextExecute(cacheId, cache -> getCurrentCache().getDependencies(ancestor).stream().map(generic -> generic.getVertex()).toArray(Vertex[]::new)) : Statics.EMPTY;
	}

	@Override
	public Vertex addInstance(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return safeContextExecute(
				cacheId,
				cache -> getCurrentCache().addInstance(getRoot().getGenericById(meta), overrides.stream().map(override -> getRoot().getGenericById(override)).collect(Collectors.toList()), value,
						components.stream().map(component -> getRoot().getGenericById(component)).collect(Collectors.toList())).getVertex());
		// TODO rollback
	}

	@Override
	public Vertex update(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> components) {
		return safeContextExecute(
				cacheId,
				cache -> getCurrentCache().update(getRoot().getGenericById(update), overrides.stream().map(override -> getRoot().getGenericById(override)).collect(Collectors.toList()), value,
						components.stream().map(component -> getRoot().getGenericById(component)).collect(Collectors.toList())).getVertex());
		// TODO rollback
	}

	@Override
	public long merge(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> components) {
		return safeContextExecute(
				cacheId,
				cache -> getCurrentCache().merge(getRoot().getGenericById(update), overrides.stream().map(override -> getRoot().getGenericById(override)).collect(Collectors.toList()), value,
						components.stream().map(component -> getRoot().getGenericById(component)).collect(Collectors.toList())).getTs());
		// TODO rollback
	}

	@Override
	public long setInstance(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return safeContextExecute(
				cacheId,
				cache -> cache.setInstance(getRoot().getGenericById(meta), overrides.stream().map(override -> getRoot().getGenericById(override)).collect(Collectors.toList()), value,
						components.stream().map(component -> getRoot().getGenericById(component)).collect(Collectors.toList())).getTs());
		// TODO rollback
	}

	private <T> T safeContextExecute(long cacheId, Function<Cache, T> function) {
		Cache cache = getCurrentCache(cacheId);
		cache.start();
		try {
			return function.apply(cache);
		} finally {
			cache.stop();
		}
	}

	@Override
	public long remove(long cacheId, long generic) {
		try {
			getCurrentCache().remove(getRoot().getGenericById(generic));
			return generic;
		} catch (Exception e) {
			return Statics.ROLLBACK_EXCEPTION;
		}
	}

	@Override
	public long forceRemove(long cacheId, long generic) {
		try {
			getCurrentCache().forceRemove(getRoot().getGenericById(generic));
			return generic;
		} catch (Exception e) {
			return Statics.ROLLBACK_EXCEPTION;
		}
	}

	@Override
	public long conserveRemove(long cacheId, long generic) {
		try {
			getCurrentCache().conserveRemove(getRoot().getGenericById(generic));
			return generic;
		} catch (Exception e) {
			return Statics.ROLLBACK_EXCEPTION;
		}
	}

	@Override
	public long flush(long cacheId) {
		try {
			getCurrentCache().flush();
			return getCurrentCache().getTs();
		} catch (Exception e) {
			return Statics.ROLLBACK_EXCEPTION;
		}
	}

	@Override
	public long tryFlush(long cacheId) {
		try {
			getCurrentCache().tryFlush();
			return getCurrentCache().getTs();
		} catch (Exception e) {
			return Statics.ROLLBACK_EXCEPTION;
		}
	}

	private final ThreadLocal<Long> contextIds = new ThreadLocal<Long>();
	private final ConcurrentHashMap<Long, AbstractCache> map = new ConcurrentHashMap<>();

	@Override
	public Cache getCurrentCache() {
		return getRoot().isInitialized() ? getCurrentCache(contextIds.get()) : (Cache) super.getCurrentCache();
	}

	@Override
	protected AbstractCache start(AbstractCache context) {
		if (!isInitialized())
			super.start(context);
		Long cacheId = context.getCacheId();
		map.put(cacheId, context);
		contextIds.set(cacheId);
	}

	@Override
	protected void stop(DefaultCache<Generic> context) {
		if (!isInitialized())
			super.stop(context);
		Long cacheId = context.getCacheId();
		contextIds.remove();
		map.remove(cacheId);
	}

	public AbstractCache getCurrentCache(Long id) {
		return map.get(id);
	}

}