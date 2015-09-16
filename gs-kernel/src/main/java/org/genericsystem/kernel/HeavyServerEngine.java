package org.genericsystem.kernel;

import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;
import org.genericsystem.common.Cache;
import org.genericsystem.common.Cache.ContextEventListener;
import org.genericsystem.common.Generic;
import org.genericsystem.common.IDifferential;
import org.genericsystem.common.Protocole.ServerCacheProtocole;
import org.genericsystem.common.Vertex;

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

	@Override
	public Cache getCurrentCache() {
		return (Cache) super.getCurrentCache();
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
	public int getCacheLevel() {
		return getCurrentCache().getCacheLevel();
	}

	@Override
	public void mount() {
		getCurrentCache().mount();

	}

	@Override
	public void unmount() {
		getCurrentCache().unmount();
	}

	@Override
	public void clear() {
		getCurrentCache().clear();
	}

	@Override
	public void shiftTs() {
		getCurrentCache().shiftTs();
	}

	@Override
	public Vertex[] getDependencies(long ts, long id) {
		Generic ancestor = this.getGenericById(id);
		return ancestor != null ? getCurrentCache().getDependencies(ancestor).stream().map(generic -> generic.getVertex()).toArray(Vertex[]::new) : Statics.EMPTY;
	}

	@Override
	public Vertex addInstance(long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return getCurrentCache().addInstance(getRoot().getGenericById(meta), overrides.stream().map(override -> getRoot().getGenericById(override)).collect(Collectors.toList()), value,
				components.stream().map(component -> getRoot().getGenericById(component)).collect(Collectors.toList())).getVertex();
	}

	@Override
	public Vertex update(long update, List<Long> overrides, Serializable value, List<Long> components) {
		return getCurrentCache().update(getRoot().getGenericById(update), overrides.stream().map(override -> getRoot().getGenericById(override)).collect(Collectors.toList()), value,
				components.stream().map(component -> getRoot().getGenericById(component)).collect(Collectors.toList())).getVertex();
	}

	@Override
	public long merge(long update, List<Long> overrides, Serializable value, List<Long> components) {
		return getCurrentCache().merge(getRoot().getGenericById(update), overrides.stream().map(override -> getRoot().getGenericById(override)).collect(Collectors.toList()), value,
				components.stream().map(component -> getRoot().getGenericById(component)).collect(Collectors.toList())).getTs();
	}

	@Override
	public long setInstance(long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return getCurrentCache().setInstance(getRoot().getGenericById(meta), overrides.stream().map(override -> getRoot().getGenericById(override)).collect(Collectors.toList()), value,
				components.stream().map(component -> getRoot().getGenericById(component)).collect(Collectors.toList())).getTs();
	}

	@Override
	public long remove(long generic) {
		try {
			getCurrentCache().remove(getRoot().getGenericById(generic));
			return generic;
		} catch (Exception e) {
			return Statics.ROLLBACK_EXCEPTION;
		}
	}

	@Override
	public long forceRemove(long generic) {
		try {
			getCurrentCache().forceRemove(getRoot().getGenericById(generic));
			return generic;
		} catch (Exception e) {
			return Statics.ROLLBACK_EXCEPTION;
		}
	}

	@Override
	public long conserveRemove(long generic) {
		try {
			getCurrentCache().conserveRemove(getRoot().getGenericById(generic));
			return generic;
		} catch (Exception e) {
			return Statics.ROLLBACK_EXCEPTION;
		}
	}

	@Override
	public long flush() {
		try {
			getCurrentCache().flush();
			return getCurrentCache().getTs();
		} catch (Exception e) {
			return Statics.ROLLBACK_EXCEPTION;
		}
	}

	@Override
	public long tryFlush() {
		try {
			getCurrentCache().tryFlush();
			return getCurrentCache().getTs();
		} catch (Exception e) {
			return Statics.ROLLBACK_EXCEPTION;
		}
	}

}