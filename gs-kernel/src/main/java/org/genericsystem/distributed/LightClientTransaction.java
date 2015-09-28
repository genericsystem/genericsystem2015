package org.genericsystem.distributed;

import java.io.Serializable;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Container;
import org.genericsystem.common.Generic;

public class LightClientTransaction {

	private LightClientEngine engine;
	private final long cacheId;
	private final long ts;

	protected LightClientTransaction(LightClientEngine engine, long cacheId, long ts) {
		this.engine = engine;
		this.cacheId = cacheId;
		this.ts = ts;
	}

	protected LightClientTransaction(LightClientEngine engine, long cacheId) {
		this(engine, cacheId, engine.pickNewTs());
	}

	public long getTs() {
		return ts;
	}

	public LightClientEngine getRoot() {
		return engine;
	}

	private Map<Generic, Snapshot<Generic>> dependenciesMap = new HashMap<>();

	public Snapshot<Generic> getDependencies(Generic generic) {
		Snapshot<Generic> dependencies = dependenciesMap.get(generic);
		if (dependencies == null) {
			dependencies = new Container(Arrays.stream(getRoot().getServer().getDependencies(cacheId, generic.getTs())).map(vertex -> getRoot().getGenericByVertex(vertex)));
			Snapshot<Generic> result = dependenciesMap.put(generic, dependencies);
			assert result == null;
		}
		return dependencies;
	}

	private Generic invalid(Generic generic) {
		// TODO deep invalid
		generic.getComponents().forEach(component -> dependenciesMap.remove(component));
		generic.getSupers().forEach(superG -> dependenciesMap.remove(superG));
		dependenciesMap.remove(generic.getMeta());
		dependenciesMap.remove(generic);
		return generic;
	}

	Generic setInstance(Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
		return invalid(getRoot().getGenericById(
				getRoot().getServer().setInstance(cacheId, meta.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList()))));
	}

	Generic addInstance(Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
		return getRoot().getGenericByVertex(
				getRoot().getServer().addInstance(cacheId, meta.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList())));
	}

	Generic update(Generic update, List<Generic> overrides, Serializable value, List<Generic> components) {
		Generic result = getRoot().getGenericByVertex(
				getRoot().getServer().update(cacheId, update.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList())));
		invalid(update);
		return result;
	}

	Generic merge(Generic update, List<Generic> overrides, Serializable value, List<Generic> components) {
		Generic result = getRoot().getGenericById(
				getRoot().getServer().merge(cacheId, update.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList())));
		invalid(update);
		return result;
	}

	void forceRemove(Generic generic) {
		getRoot().getServer().forceRemove(cacheId, generic.getTs());
		invalid(generic);
	}

	void remove(Generic generic) {
		getRoot().getServer().remove(cacheId, generic.getTs());
		invalid(generic);
	}

	void conserveRemove(Generic generic) {
		getRoot().getServer().conserveRemove(cacheId, generic.getTs());
		invalid(generic);
	}

	public int getCacheLevel() {
		return getRoot().getServer().getCacheLevel(cacheId);
	}
}
