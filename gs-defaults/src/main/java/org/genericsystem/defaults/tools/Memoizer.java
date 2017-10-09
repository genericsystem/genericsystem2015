package org.genericsystem.defaults.tools;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

public class Memoizer {

	public static <T, U> Function<T, U> memoize(Function<T, U> function) {
		Map<T, U> cache = new HashMap<>();
		return t -> {
			if (cache.containsKey(t))
				return cache.get(t);
			U result = function.apply(t);
			cache.put(t, result);
			return result;
		};
	}
}
