package org.genericsystem.distributed.cacheonserver;

import java.io.Serializable;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NavigableSet;
import java.util.stream.Collectors;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Container;
import org.genericsystem.common.Generic;

public class CosTransaction {

	private CosClientEngine engine;
	private final long cacheId;

	// private final long ts;

	// protected LightClientTransaction(LightClientEngine engine, long cacheId, long ts) {
	// this.engine = engine;
	// this.cacheId = cacheId;
	// // this.ts = ts;
	// }

	protected CosTransaction(CosClientEngine engine, long cacheId) {
		this.engine = engine;
		this.cacheId = cacheId;
	}

	// public long getTs() {
	// return ts;
	// }

	public CosClientEngine getRoot() {
		return engine;
	}

	private Map<Generic, Snapshot<Generic>> dependenciesMap = new HashMap<>();

	public Snapshot<Generic> getDependencies(Generic generic) {
		return () -> {
			Snapshot<Generic> dependencies = dependenciesMap.get(generic);
			if (dependencies == null) {
				dependencies = new Container(Arrays.stream(getRoot().getServer().getDependencies(cacheId, generic.getTs())).map(vertex -> getRoot().getGenericByVertex(vertex)));
				Snapshot<Generic> result = dependenciesMap.put(generic, dependencies);
				assert result == null;
			}
			return dependencies.stream();
		};
	}

	private Generic evict(Generic generic) {
		generic.getComponents().forEach(component -> dependenciesMap.remove(component));
		generic.getSupers().forEach(superG -> dependenciesMap.remove(superG));
		dependenciesMap.remove(generic.getMeta());
		dependenciesMap.remove(generic);
		return generic;
	}

	Generic setInstance(Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
		return evict(getRoot().getGenericById(
				getRoot().getServer().setInstance(cacheId, meta.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList()))));
	}

	Generic addInstance(Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
		return evict(getRoot().getGenericById(
				getRoot().getServer().addInstance(cacheId, meta.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList()))));
	}

	Generic update(Generic update, List<Generic> overrides, Serializable value, List<Generic> components) {
		Generic result = getRoot().getGenericById(
				getRoot().getServer().update(cacheId, update.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList())));
		evict(update);
		return result;
	}

	Generic merge(Generic update, List<Generic> overrides, Serializable value, List<Generic> components) {
		Generic result = getRoot().getGenericById(
				getRoot().getServer().merge(cacheId, update.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList())));
		evict(update);
		return result;
	}

	void forceRemove(Generic generic, NavigableSet<Generic> dependencies) {
		getRoot().getServer().forceRemove(cacheId, generic.getTs());
		dependencies.forEach(g -> evict(g));
	}

	long remove(Generic generic, NavigableSet<Generic> dependencies) {
		long result = getRoot().getServer().remove(cacheId, generic.getTs());
		dependencies.forEach(g -> evict(g));
		return result;
	}

	void conserveRemove(Generic generic, NavigableSet<Generic> dependencies) {
		getRoot().getServer().conserveRemove(cacheId, generic.getTs());
		dependencies.forEach(g -> evict(g));
	}

	public int getCacheLevel() {
		return getRoot().getServer().getCacheLevel(cacheId);
	}
}
