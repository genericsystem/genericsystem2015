package org.genericsystem.defaults.tools;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;

import javafx.beans.InvalidationListener;
import javafx.beans.WeakInvalidationListener;
import javafx.collections.ObservableList;

import com.sun.javafx.collections.ObservableListWrapper;

@SuppressWarnings("restriction")
public abstract class MinimalChangesObservableList<E> extends ObservableListWrapper<E> {

	private final ObservableList<E> internal;

	private final InvalidationListener listener = o -> {
		beginChange();
		doMinimalChanges();
		System.out.println("invalidate");
		endChange();
	};

	MinimalChangesObservableList(ObservableList<E> internal) {
		super(new ArrayList<>());
		this.internal = internal;
		this.internal.addListener(new WeakInvalidationListener(listener));
	}

	protected void doMinimalChanges() {
		List<E> oldList = new ArrayList<>(this);
		List<E> newList = internal;
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
				if (e.getValue())
					add(size(), e.getKey());
				else
					remove(index);
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
