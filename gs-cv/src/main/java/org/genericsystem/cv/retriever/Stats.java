package org.genericsystem.cv.retriever;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public final class Stats {
	private static Map<String, List<Long>> cumulativeTimers = new ConcurrentHashMap<>();
	private static Map<String, Long> cumul = new ConcurrentHashMap<>();

	private static Map<String, Long> timers = new ConcurrentHashMap<>();
	private static Map<String, List<Long>> stats = new ConcurrentHashMap<>();

	public static void beginCumulative(String key) {
		cumul.put(key, System.nanoTime());
	}

	public static void endCumulative(String key) {
		long value;
		if (cumul.get(key) != null)
			value = System.nanoTime() - cumul.get(key);
		else
			value = 0L;
		List<Long> times = cumulativeTimers.get(key);
		if (times == null)
			times = new ArrayList<>();
		times.add(value);
		cumulativeTimers.put(key, times);
	}

	public static void resetCumulative(String key) {
		List<Long> timers = cumulativeTimers.get(key);
		long total = timers.stream().mapToLong(x -> x).sum();
		cumulativeTimers.put(key, new ArrayList<>());
		List<Long> times = stats.get(key);
		if (times == null)
			times = new ArrayList<>();
		times.add(total);
		stats.put(key, times);
	}

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
		cumulativeTimers = new ConcurrentHashMap<>();
		timers = new ConcurrentHashMap<>();
		stats = new ConcurrentHashMap<>();
	}

	public static String getStatsAndReset() {
		if (stats.isEmpty())
			return "";
		StringBuffer sb = new StringBuffer();
		sb.append("\n").append("###  STATS  ###").append("\n");
		stats.entrySet().forEach(entry -> {
			double average = entry.getValue().stream().mapToDouble(l -> l.doubleValue()).average().getAsDouble();
			double sem = Math.sqrt(entry.getValue().stream().mapToDouble(x -> Math.pow(x - average, 2)).sum() / (entry.getValue().size() - 1));

			sb.append(entry.getKey()).append(": ");
			sb.append(String.format("%.3f", average / 1_000_000));
			sb.append(" ± ").append(String.format("%.3f", sem / 1_000_000));
			sb.append("ms");
			// sb.append(" | ").append(entry.getValue().stream().map(x -> x / 1_000_000).collect(Collectors.toList()));
			sb.append("\n");
		});
		sb.append("###  -----  ###").append("\n");
		reset();
		return sb.toString();
	}
}
