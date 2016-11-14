package org.genericsystem.defaults.tools;

import java.lang.ref.WeakReference;
import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;

import com.sun.javafx.collections.ObservableListWrapper;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 * @param <E>
 */

public abstract class AbstractMinimalChangesObservableList<E> extends ObservableListWrapper<E> {

	private BindingHelperObserver observer;

	protected void invalidate() {
		// beginChange();
		doMinimalChanges();
		// endChange();
	}

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

	public AbstractMinimalChangesObservableList() {
		super(new ArrayList<>());
	}

	protected abstract List<E> computeValue();

	protected void doMinimalChanges() {
		List<E> newList = computeValue();
		Diff<E> diff = new Diff<>(new ArrayList<>(this), newList);
		int index = 0;
		while (diff.hasNext()) {
			Entry<E, Boolean> e = diff.next();
			if (e.getValue() == null) {
				// System.out.println("Nop");
				index++;
			} else {
				// System.out.println((e.getValue() ? "Add : " + e.getKey() : "Remove : " + e.getKey()) + " index : " + index);
				if (e.getValue())
					add(index++, e.getKey());
				else
					remove(index);
			}
		}
	}

	public static class MinimalChangesObservableList<E> extends AbstractMinimalChangesObservableList<E> {

		private final ObservableList<E> observableList;

		public MinimalChangesObservableList(ObservableList<E> observableList) {
			this.observableList = observableList;
			bind(observableList);
			invalidate();
		}

		@Override
		protected List<E> computeValue() {
			return observableList;
		}
	}

	private static class BindingHelperObserver implements InvalidationListener {

		private final WeakReference<AbstractMinimalChangesObservableList<?>> ref;

		public BindingHelperObserver(AbstractMinimalChangesObservableList<?> binding) {
			if (binding == null) {
				throw new NullPointerException("Binding has to be specified.");
			}
			ref = new WeakReference<>(binding);
		}

		@Override
		public void invalidated(Observable observable) {
			final AbstractMinimalChangesObservableList<?> binding = ref.get();
			if (binding == null) {
				assert false;
				observable.removeListener(this);
			} else {
				binding.invalidate();
			}
		}

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
