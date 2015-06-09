package org.genericsystem.kernel;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Vertex {

	private final long ts;
	private final long meta;
	private final List<Long> supers;
	private final Serializable value;
	private final List<Long> components;
	private final LifeManager lifeManager;

	protected Vertex(long ts, long meta, List<Long> supers, Serializable value, List<Long> components, LifeManager lifeManager) {
		this.ts = ts;
		this.meta = meta;
		this.value = value;
		for (Long component : components)
			assert component != null && !equals(component);
		this.components = Collections.unmodifiableList(new ArrayList<>(components));
		this.supers = Collections.unmodifiableList(new ArrayList<>(supers));
		this.lifeManager = lifeManager;
	}

	public long getTs() {
		return ts;
	}

	public long getBirthTs() {
		return lifeManager.getBirthTs();
	}

	public long getDeathTs() {
		return lifeManager.getDeathTs();
	}

	public long getMeta() {
		return meta;
	}

	public List<Long> getSupers() {
		return supers;
	}

	public Serializable getValue() {
		return value;
	}

	public List<Long> getComponents() {
		return components;
	}

	public LifeManager getLifeManager() {
		return lifeManager;
	}

	// AbstractTsDependencies getDependencies() {
	// return dependencies;
	// }

	// Long getNextDependency(Long ancestor) {
	// return nextDependencies.get(ancestor);
	// }
	//
	// void setNextDependency(Long ancestor, Long nextDependency) {
	// nextDependencies.put(ancestor, nextDependency);
	// }

}
