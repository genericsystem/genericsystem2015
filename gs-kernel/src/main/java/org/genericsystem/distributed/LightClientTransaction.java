package org.genericsystem.distributed;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Container;
import org.genericsystem.common.Generic;

public class LightClientTransaction {

	private LightClientEngine engine;
	private final long ts;

	protected LightClientTransaction(LightClientEngine engine, long ts) {
		this.engine = engine;
		this.ts = ts;
	}

	protected LightClientTransaction(LightClientEngine engine) {
		this(engine, engine.pickNewTs());
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
			dependencies = new Container(Arrays.stream(getRoot().getServer().getDependencies(getTs(), generic.getTs())).map(vertex -> getRoot().getGenericByVertex(vertex)));
			Snapshot<Generic> result = dependenciesMap.put(generic, dependencies);
			assert result == null;
		}
		return dependencies;
	}

}
