package org.genericsystem.cv.classifier;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public final class Stats {
	private static Map<String, Long> timers = new HashMap<>();
	private static Map<String, List<Long>> stats = new HashMap<>();

	public static void beginTask(String key) {
		timers.put(key, System.nanoTime());
	}

	public static void endTask(String key) {
		long value;
		if (timers.get(key) != null)
			value = System.nanoTime() - timers.get(key);
		else
			value = 0L;
		List<Long> times = stats.get(key);
		if (times == null)
			times = new ArrayList<>();
		times.add(value);
		stats.put(key, times);
	}

	public static void reset() {
		timers = new HashMap<>();
		stats = new HashMap<>();
	}

	public static String getStatsAndReset() {
		if (stats.isEmpty())
			return "";
		StringBuffer sb = new StringBuffer();
		sb.append("\n").append("###  STATS  ###").append("\n");
		stats.entrySet().forEach(entry -> sb.append(entry.getKey()).append(": ").append(entry.getValue().stream().mapToDouble(l -> l.doubleValue()).average().getAsDouble() / 1_000_000).append("ms").append("\n"));
		sb.append("######").append("\n");
		reset();
		return sb.toString();
	}
}
