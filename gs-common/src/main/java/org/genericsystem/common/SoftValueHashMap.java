package org.genericsystem.common;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.SoftReference;
import java.util.AbstractMap;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

public class SoftValueHashMap<K, V> extends AbstractMap<K, V> implements Map<K, V> {

	private Map<K, SoftValueRef<K, V>> map = new HashMap<>();

	private ReferenceQueue<V> queue = new ReferenceQueue<>();

	private static class SoftValueRef<K, V> extends SoftReference<V> {
		public K key;

		private SoftValueRef(K key, V val, ReferenceQueue<V> q) {
			super(val, q);
			this.key = key;
		}

		@SuppressWarnings({ "unchecked", "rawtypes" })
		private static <T, U> SoftValueRef<T, U> create(T key, U val, ReferenceQueue<U> q) {
			if (val == null)
				return null;
			else
				return new SoftValueRef(key, val, q);
		}

		@Override
		public boolean equals(Object obj) {
			if (!(obj instanceof SoftValueRef))
				return false;
			return Objects.equals(((SoftValueRef<?, ?>) obj).get(), get());
		}
	}

	@Override
	public synchronized Set entrySet() {
		processQueue();
		return map.entrySet();
	}

	@Override
	public synchronized Collection<V> values() {
		processQueue();
		return map.values().stream().map(ref -> ref.get()).filter(v -> v != null).collect(Collectors.toList());
	}

	@SuppressWarnings("unchecked")
	private synchronized void processQueue() {
		SoftValueRef<K, V> ref;
		while ((ref = (SoftValueRef<K, V>) queue.poll()) != null)
			map.remove(ref.key);
	}

	@Override
	public synchronized int size() {
		processQueue();
		return map.size();
	}

	@Override
	public synchronized boolean isEmpty() {
		processQueue();
		return map.isEmpty();
	}

	@Override
	public synchronized boolean containsKey(Object key) {
		processQueue();
		return map.containsKey(key);
	}

	@Override
	public synchronized V get(Object key) {
		processQueue();
		SoftValueRef<K, V> ref = map.get(key);
		return ref == null ? null : ref.get();
	}

	@Override
	public synchronized V put(K key, V value) {
		processQueue();
		SoftValueRef<K, V> ref = map.put(key, SoftValueRef.create(key, value, queue));
		return ref == null ? null : ref.get();
	}

	@Override
	public synchronized V remove(Object key) {
		processQueue();
		SoftValueRef<K, V> ref = map.remove(key);
		return ref == null ? null : ref.get();
	}

	@Override
	public synchronized void clear() {
		processQueue();
		map.clear();
	}

	@Override
	public synchronized V putIfAbsent(K key, V value) {
		processQueue();
		V result = get(key);
		if (result == null) {
			result = put(key, value);
		}
		return result;
	}

	@Override
	public synchronized V computeIfAbsent(K key, Function<? super K, ? extends V> mappingFunction) {
		processQueue();
		V result = get(key);
		if (result == null) {
			V newValue = mappingFunction.apply(key);
			put(key, newValue);
			return newValue;
		}
		return result;
	}
}
