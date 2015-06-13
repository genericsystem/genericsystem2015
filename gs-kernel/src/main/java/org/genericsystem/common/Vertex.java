package org.genericsystem.common;

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
	private final long[] otherTs;

	protected Vertex(long ts, long meta, List<Long> supers, Serializable value, List<Long> components, long[] otherTs) {
		this.ts = ts;
		this.meta = meta;
		this.value = value;
		for (Long component : components)
			assert component != null && !equals(component);
		this.components = Collections.unmodifiableList(new ArrayList<>(components));
		this.supers = Collections.unmodifiableList(new ArrayList<>(supers));
		this.otherTs = otherTs;
	}

	public long getTs() {
		return ts;
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

	public long[] getOtherTs() {
		return otherTs;
	}
}
