package org.genericsystem.common;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Spliterators;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Predicate;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.genericsystem.api.core.Filters;
import org.genericsystem.api.core.Filters.IndexFilter;
import org.genericsystem.api.core.IGeneric;
import org.genericsystem.api.core.IteratorSnapshot;
import org.genericsystem.api.core.Snapshot;

import javafx.beans.Observable;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.WeakChangeListener;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public class PseudoConcurrentCollection<T> implements IteratorSnapshot<T> {
	private static interface Index<T> {
		public void add(T generic);

		public boolean remove(T generic);

		public Iterator<T> iterator();

		public Stream<T> stream();
	}

	final Map<T, T> map = new HashMap<>();

	@SuppressWarnings("unchecked")
	private final ConcurrentHashMap<IndexFilter<T>, Index<T>> indexs = new ConcurrentHashMap<IndexFilter<T>, Index<T>>() {
		{
			put((IndexFilter<T>) Filters.NO_FILTER, new IndexImpl((IndexFilter<T>) Filters.NO_FILTER));
		}

		private static final long serialVersionUID = 6592932243687280301L;

		@Override
		public Index<T> get(Object key) {
			return super.computeIfAbsent((IndexFilter<T>) key, k -> new IndexImpl(k));
		};
	};

	private class IndexImpl implements Index<T> {
		private Node<T> head = null;
		private Node<T> tail = null;

		IndexImpl(IndexFilter<T> filter) {
			if (!Filters.NO_FILTER.equals(filter))
				indexs.get(Filters.NO_FILTER).iterator().forEachRemaining(generic -> {
					if (filter.test(generic))
						add(generic);
				});
		}

		@Override
		public void add(T element) {
			assert element != null;
			Node<T> newNode = new Node<>(element);
			if (head == null)
				head = newNode;
			else
				tail.next = newNode;
			tail = newNode;
			map.put(element, element);
		}

		@Override
		public boolean remove(T element) {
			Iterator<T> iterator = iterator();
			while (iterator.hasNext())
				if (element.equals(iterator.next())) {
					iterator.remove();
					return true;
				}
			return false;
		}

		@Override
		public Iterator<T> iterator() {
			return new InternalIterator();
		}

		@Override
		public Stream<T> stream() {
			return StreamSupport.stream(Spliterators.spliteratorUnknownSize(new InternalIterator(), 0), false);
		}

		public class InternalIterator extends AbstractIterator<Node<T>, T> implements Iterator<T> {

			private Node<T> last;

			@Override
			protected void advance() {
				last = next;
				next = next == null ? head : next.next;
			}

			@Override
			public T project() {
				return next.content;
			}

			@Override
			public void remove() {
				T content = next.content;
				if (next == null)
					throw new IllegalStateException();
				map.remove(next.content);
				if (last == null) {
					head = next.next;
					next = null;
				} else {
					last.next = next.next;
					if (next.next == null)
						tail = last;
				}
			}
		}
	}

	private static class Node<T> {
		private final T content;
		private Node<T> next;

		private Node(T content) {
			this.content = content;
		}
	}

	@Override
	public Iterator<T> iterator() {
		return indexs.get(Filters.NO_FILTER).iterator();
	}

	public Stream<T> stream(IndexFilter<T> filter) {
		return indexs.get(filter).stream();
	}

	@SuppressWarnings("unchecked")
	@Override
	public Stream<T> stream() {
		return stream((IndexFilter<T>) Filters.NO_FILTER);
	}

	@SuppressWarnings("unchecked")
	@Override
	public <U extends IGeneric<U>> Snapshot<T> filter(Filters filter, U... generics) {
		return new Snapshot<T>() {

			@Override
			public Stream<T> stream() {
				return PseudoConcurrentCollection.this.stream(filter.getFilter(generics));
			}
		};
	}

	public void add(T element) {
		indexs.entrySet().forEach(entry -> {
			if (entry.getKey().test(element)) {
				entry.getValue().add(element);
			}
		});
		addProperty.set(element);
	}

	public boolean remove(T element) {
		boolean[] result = new boolean[] { false };
		indexs.entrySet().forEach(entry -> {
			if (entry.getKey().test(element))
				result[0] = result[0] | entry.getValue().remove(element);
		});
		removeProperty.set(element);
		return result[0];
	}

	@Override
	public T get(Object o) {
		return map.get(o);
	}

	private SimpleObjectProperty<T> addProperty = new SimpleObjectProperty<T>();
	private SimpleObjectProperty<T> removeProperty = new SimpleObjectProperty<T>();

	private class FilteredInvalidator extends SimpleObjectProperty<T> {
		private Predicate<T> predicate;

		private ChangeListener<T> listener = (o, oldT, newT) -> {
			if (fireInvalidations && predicate.test(newT))
				super.fireValueChangedEvent();
		};

		private FilteredInvalidator(Predicate<T> predicate) {
			this.predicate = predicate;
			addProperty.addListener(new WeakChangeListener<T>(listener));
			removeProperty.addListener(new WeakChangeListener<T>(listener));
		}

	}

	private boolean fireInvalidations = true;

	public Observable getFilteredInvalidator(T generic, Predicate<T> predicate) {
		return new FilteredInvalidator(predicate);
	}

	public void disableInvalidations() {
		fireInvalidations = false;
	}

	public void enableInvalidations() {
		fireInvalidations = true;
	}
}
