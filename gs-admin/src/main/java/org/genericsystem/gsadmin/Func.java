package org.genericsystem.gsadmin;

import java.util.HashMap;
import java.util.Map;

public class Func extends HashMap<Class<?>, Map<Class<?>, Object>> {
	private static final long serialVersionUID = -7558745441434380361L;

	private static Func staticMap = new Func();

	private static Func instance() {
		return staticMap;
	}

	private Func() {
	}

	public static <T> T get(Class<?> modelClass, Class<T> functionalInterface) {
		return instance().getInternal(modelClass, functionalInterface);
	}

	private <T> T getInternal(Class<?> modelClass, Class<T> functionalInterface) {
		for (Class<?> internalModelClass = modelClass; internalModelClass != null; internalModelClass = internalModelClass.getSuperclass()) {
			System.out.println("find : " + internalModelClass);
			Map<Class<?>, Object> map = super.get(modelClass);
			if (map != null) {
				Object lambda = map.get(functionalInterface);
				if (lambda != null)
					return (T) lambda;
			}
		}
		throw new IllegalStateException("Unnable to find a lamda for model class : " + modelClass + " for functional interface : " + functionalInterface);
	}

	public static <T> void put(Class<?> modelClass, Class<T> functionalInterface, T lamda) {
		instance().putInternal(modelClass, functionalInterface, lamda);
	}

	public <T> void putInternal(Class<?> modelClass, Class<? extends T> functionalInterface, T lamda) {
		Map<Class<?>, Object> map = super.get(modelClass);
		if (map == null)
			put(modelClass, map = new HashMap<Class<?>, Object>());
		map.put(functionalInterface, lamda);
	}
}