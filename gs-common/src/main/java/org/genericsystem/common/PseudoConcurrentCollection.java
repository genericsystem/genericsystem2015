package org.genericsystem.common;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.function.Predicate;

import org.genericsystem.api.core.IteratorSnapshot;

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
		addProperty.set(element);
	}

	public boolean remove(T element) {
		Iterator<T> iterator = iterator();
		while (iterator.hasNext())
			if (element.equals(iterator.next())) {
				iterator.remove();
				// map.remove(element);
				return true;
			}
		return false;
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
			removeProperty.set(content);
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
