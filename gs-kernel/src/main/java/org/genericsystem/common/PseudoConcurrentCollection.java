package org.genericsystem.common;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.function.Predicate;

import javafx.beans.Observable;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.beans.value.WeakChangeListener;

import org.genericsystem.api.core.IteratorSnapshot;

public class PseudoConcurrentCollection<T> implements IteratorSnapshot<T> {

	// TODO size and get(index) !!!
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
		changeProperty.set(element);
		;
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
			changeProperty.set(content);
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

	private SimpleObjectProperty<T> changeProperty = new SimpleObjectProperty<T>();

	private class FilteredInvalidator extends SimpleObjectProperty<T> implements ChangeListener<T> {
		private Predicate<T> predicate;

		private FilteredInvalidator(Predicate<T> predicate) {
			this.predicate = predicate;
			changeProperty.addListener(new WeakChangeListener<T>(this));
		}

		@Override
		public void changed(ObservableValue<? extends T> observable, T oldValue, T newValue) {
			if (predicate.test(newValue))
				super.fireValueChangedEvent();
		}
	}

	public Observable getFilteredInvalidator(T generic, Predicate<T> predicate) {
		return new FilteredInvalidator(predicate);
	}
}
