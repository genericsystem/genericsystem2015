package org.genericsystem.defaults.tools;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.binding.Binding;
import javafx.beans.binding.ListExpression;
import javafx.beans.property.ReadOnlyBooleanProperty;
import javafx.beans.property.ReadOnlyBooleanPropertyBase;
import javafx.beans.property.ReadOnlyIntegerProperty;
import javafx.beans.property.ReadOnlyIntegerPropertyBase;
import javafx.beans.value.ChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import com.sun.javafx.binding.BindingHelperObserver;
import com.sun.javafx.binding.ListExpressionHelper;

@SuppressWarnings("restriction")
public abstract class MinimalChangesListBinding<E> extends ListExpression<E> implements Binding<ObservableList<E>> {

	private final ListChangeListener<E> listChangeListener = new ListChangeListener<E>() {
		@Override
		public void onChanged(Change<? extends E> change) {
			System.out.println("MinimalChangesListBinding internal CHANGE :" + change);
			ListExpressionHelper.fireValueChangedEvent(helper, change);
		}
	};

	public MinimalChangesListBinding() {
		value.addListener(listChangeListener);
		invalidate();
	}

	private final ObservableList<E> value = FXCollections.observableArrayList();
	// private boolean valid = false;
	private BindingHelperObserver observer;
	private ListExpressionHelper<E> helper = null;

	private SizeProperty size0;
	private EmptyProperty empty0;

	@Override
	public ReadOnlyIntegerProperty sizeProperty() {
		if (size0 == null) {
			size0 = new SizeProperty();
		}
		return size0;
	}

	private class SizeProperty extends ReadOnlyIntegerPropertyBase {
		@Override
		public int get() {
			return size();
		}

		@Override
		public Object getBean() {
			return MinimalChangesListBinding.this;
		}

		@Override
		public String getName() {
			return "size";
		}

		@Override
		protected void fireValueChangedEvent() {
			super.fireValueChangedEvent();
		}
	}

	@Override
	public ReadOnlyBooleanProperty emptyProperty() {
		if (empty0 == null) {
			empty0 = new EmptyProperty();
		}
		return empty0;
	}

	private class EmptyProperty extends ReadOnlyBooleanPropertyBase {

		@Override
		public boolean get() {
			return isEmpty();
		}

		@Override
		public Object getBean() {
			return MinimalChangesListBinding.this;
		}

		@Override
		public String getName() {
			return "empty";
		}

		@Override
		protected void fireValueChangedEvent() {
			super.fireValueChangedEvent();
		}
	}

	@Override
	public void addListener(InvalidationListener listener) {
		helper = ListExpressionHelper.addListener(helper, this, listener);
	}

	@Override
	public void removeListener(InvalidationListener listener) {
		helper = ListExpressionHelper.removeListener(helper, listener);
	}

	@Override
	public void addListener(ChangeListener<? super ObservableList<E>> listener) {
		helper = ListExpressionHelper.addListener(helper, this, listener);
	}

	@Override
	public void removeListener(ChangeListener<? super ObservableList<E>> listener) {
		helper = ListExpressionHelper.removeListener(helper, listener);
	}

	@Override
	public void addListener(ListChangeListener<? super E> listener) {
		helper = ListExpressionHelper.addListener(helper, this, listener);
	}

	@Override
	public void removeListener(ListChangeListener<? super E> listener) {
		helper = ListExpressionHelper.removeListener(helper, listener);
	}

	/**
	 * Start observing the dependencies for changes. If the value of one of the dependencies changes, the binding is marked as invalid.
	 *
	 * @param dependencies
	 *            the dependencies to observe
	 */
	protected final void bind(Observable... dependencies) {
		if ((dependencies != null) && (dependencies.length > 0)) {
			if (observer == null) {
				observer = new BindingHelperObserver(this);
			}
			for (final Observable dep : dependencies) {
				if (dep != null) {
					dep.addListener(observer);
				}
			}
		}
	}

	/**
	 * Stop observing the dependencies for changes.
	 *
	 * @param dependencies
	 *            the dependencies to stop observing
	 */
	protected final void unbind(Observable... dependencies) {
		if (observer != null) {
			for (final Observable dep : dependencies) {
				if (dep != null) {
					dep.removeListener(observer);
				}
			}
			observer = null;
		}
	}

	/**
	 * A default implementation of {@code dispose()} that is empty.
	 */
	@Override
	public void dispose() {}

	/**
	 * A default implementation of {@code getDependencies()} that returns an empty {@link javafx.collections.ObservableList}.
	 *
	 * @return an empty {@code ObservableList}
	 */
	@Override
	public ObservableList<?> getDependencies() {
		return FXCollections.emptyObservableList();
	}

	/**
	 * Returns the result of {@link #computeValue()}. The method {@code computeValue()} is only called if the binding is invalid. The result is cached and returned if the binding did not become invalid since the last call of {@code get()}.
	 *
	 * @return the current value
	 */
	@Override
	public final ObservableList<E> get() {
		return value;
	}

	@Override
	public final void invalidate() {
		doMinimalChanges();
	}

	@Override
	public final boolean isValid() {
		return true;
	}

	/**
	 * Calculates the current value of this binding.
	 * <p>
	 * Classes extending {@code ListBinding} have to provide an implementation of {@code computeValue}.
	 *
	 * @return the current value
	 */
	protected abstract List<E> computeValue();

	/**
	 * Returns a string representation of this {@code ListBinding} object.
	 *
	 * @return a string representation of this {@code ListBinding} object.
	 */
	@Override
	public String toString() {
		return "ListBinding [value: " + get() + "]";
	}

	protected void doMinimalChanges() {
		List<E> oldList = new ArrayList<>(value);
		List<E> newList = computeValue();
		System.out.println("OLD" + oldList);
		System.out.println("NEW" + newList);
		Diff<E> diff = new Diff<>(oldList, newList);
		int index = 0;
		while (diff.hasNext()) {
			Entry<E, Boolean> e = diff.next();
			if (e.getValue() == null) {
				System.out.println("Nop");
				index++;
			} else {
				System.out.println((e.getValue() ? "Add : " + e.getKey() : "Remove : " + e.getKey()) + " index : " + index);
				if (e.getValue()) {
					value.add(index++, e.getKey());
				} else
					value.remove(index);
			}
		}
		assert value.equals(newList);
	}

	private static class Diff<E> implements Iterator<Entry<E, Boolean>> {
		private final int m;
		private final int n;
		private final List<E> elements1;
		private final List<E> elements2;
		private int i = 0;
		private int j = 0;
		private Matrix opt;
		private Entry<E, Boolean> next;

		public Diff(List<E> elements1, List<E> elements2) {
			this.elements1 = elements1;
			this.elements2 = elements2;
			m = elements1.size();
			n = elements2.size();
			opt = new Matrix(m + 1, n + 1);
			if (m > 0 && n > 0) {
				for (int i = m - 1; i >= 0; i--) {
					for (int j = n - 1; j >= 0; j--) {
						E x = elements1.get(i);
						E y = elements2.get(j);
						opt.set(i, j, x.equals(y) ? (opt.get(i + 1, j + 1) + 1) : Math.max(opt.get(i + 1, j), opt.get(i, j + 1)));
					}
				}
			}
			next = advance();
		}

		@Override
		public boolean hasNext() {
			return next != null;
		}

		private Entry<E, Boolean> advance() {
			if (i < m && j < n) {
				E e1 = elements1.get(i);
				E e2 = elements2.get(j);
				if (e1.equals(e2)) {
					i++;
					j++;
					return new SimpleEntry<>(e1, null);
				} else if (opt.get(i + 1, j) >= opt.get(i, j + 1)) {
					i++;
					return new SimpleEntry<>(e1, false);
				} else {
					j++;
					return new SimpleEntry<>(e2, true);
				}
			} else if (i < m) {
				E e1 = elements1.get(i);
				i++;
				return new SimpleEntry<>(e1, false);
			} else if (j < n) {
				E e2 = elements2.get(j);
				j++;
				return new SimpleEntry<>(e2, true);
			} else {
				return null;
			}
		}

		@Override
		public Entry<E, Boolean> next() {
			Entry<E, Boolean> result = next;
			next = advance();
			return result;
		}

		private class Matrix {
			private final int[] state;

			Matrix(Integer width, Integer height) {
				state = new int[width * height];
				Arrays.fill(state, 0);
			}

			public void set(Integer x, Integer y, Integer e) {
				state[x + y * (m + 1)] = e;
			}

			public int get(Integer x, Integer y) {
				return state[x + y * (m + 1)];
			}
		}

	}

}
