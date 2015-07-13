package org.genericsystem.common;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Vertex implements Serializable {

	private static final long serialVersionUID = 8227404973479376042L;
	private final Class<?> clazz;
	private final long ts;
	private final long meta;
	private final List<Long> supers;
	private final Serializable value;
	private final List<Long> components;
	private final long[] otherTs;

	protected Vertex(Class<?> clazz, long ts, long meta, List<Long> supers, Serializable value, List<Long> components, long[] otherTs) {
		assert !Object.class.equals(clazz);
		this.clazz = clazz;
		this.ts = ts;
		this.meta = meta;
		this.value = value;
		for (Long component : components)
			assert component != null && !equals(component);
		this.components = Collections.unmodifiableList(new ArrayList<>(components));
		this.supers = Collections.unmodifiableList(new ArrayList<>(supers));
		this.otherTs = otherTs.clone();
	}

	// public Vertex(JsonObject json) throws ClassNotFoundException {
	// this((Class) Class.forName(json.getString("class")), (long) json.getLong("ts"), (long) json.getLong("meta"), (List<Long>) json.getJsonArray("supers").getList(), (Serializable) json.getValue("value"), (List<Long>) json.getJsonArray("components")
	// .getList(), json.getJsonArray("otherTs").getList().stream().mapToLong(l -> (Long) l).toArray());
	// }

	public Class<?> getClazz() {
		return clazz;
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

	// public JsonObject getJsonObject() {
	// JsonObject json = new JsonObject();
	// json.put("class", clazz.getName());
	// json.put("ts", ts);
	// json.put("meta", meta);
	// AssertionError f;
	// assert !(value instanceof Class) : value + "   " + clazz.getName();
	// json.put("value", value);
	// json.put("components", new JsonArray(components));
	// json.put("supers", new JsonArray(supers));
	// json.put("otherTs", new JsonArray(LongStream.of(otherTs).mapToObj(lo -> Long.valueOf(lo)).collect(Collectors.toList())));
	// return json;
	// }
}
