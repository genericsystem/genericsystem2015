package org.genericsystem.cache;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.function.Predicate;

import org.genericsystem.api.core.IteratorSnapshot;
import org.genericsystem.kernel.iterator.AbstractGeneralAwareIterator;

public class PseudoConcurrentCollection<T> implements IteratorSnapshot<T> {

	private Node<T> head = null;
	private Node<T> tail = null;
	final Map<T, T> map = new HashMap<>();

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

	public boolean remove(T element) {
		Iterator<T> iterator = iterator();
		while (iterator.hasNext())
			if (element.equals(iterator.next())) {
				iterator.remove();
				map.remove(element);
				return true;
			}
		return false;
	}

	public boolean removeIf(Predicate<? super T> filter) {
		Objects.requireNonNull(filter);
		boolean removed = false;
		final Iterator<T> each = iterator();
		while (each.hasNext()) {
			if (filter.test(each.next())) {
				each.remove();
				removed = true;
			}
		}
		return removed;
	}

	public class InternalIterator extends AbstractGeneralAwareIterator<Node<T>, T> implements Iterator<T> {

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
			if (next == null)
				throw new IllegalStateException();
			if (last == null) {
				head = next.next;
				return;
			}
			last.next = next.next;
			if (next.next == null)
				tail = last;
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
		return new InternalIterator();
	}

	@Override
	public T get(Object o) {
		return map.get(o);
	}
}
