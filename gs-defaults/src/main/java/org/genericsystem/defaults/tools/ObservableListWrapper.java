package org.genericsystem.defaults.tools;

import java.lang.ref.WeakReference;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.ListIterator;
import java.util.RandomAccess;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import javafx.beans.Observable;
import javafx.collections.ListChangeListener;
import javafx.collections.ModifiableObservableListBase;
import javafx.collections.ObservableList;
import javafx.util.Callback;

public class ObservableListWrapper<E> extends ModifiableObservableListBase<E>
		implements ObservableList<E>, SortableList<E>, RandomAccess, ListChangeListener<E> {

	private final List<E> backingList;
	private final ElementObserver<E> elementObserver;

	private final BiConsumer<Integer, E> addBiConsumer;
	private final ObservableList<E> external;
	private final Consumer<Integer> removeConsumer;
	private BindingHelperObserver<E> observer = new BindingHelperObserver<>(this);

	public ObservableListWrapper(ObservableList<E> external, Callback<E, Observable[]> extractor) {
		backingList = new ArrayList<>();
		this.elementObserver = new ElementObserver<>(extractor, e -> o -> {
			beginChange();
			int i = 0;
			final int size = size();
			for (; i < size; ++i) {
				if (get(i) == e) {
					nextUpdate(i);
				}
			}
			endChange();
		}, this);

		final int sz = backingList.size();
		for (int i = 0; i < sz; ++i) {
			elementObserver.attachListener(backingList.get(i));
		}

		this.external = external; // prevents of listener garbage collection
		this.addBiConsumer = (index, src) -> add(index, src);
		this.removeConsumer = index -> remove(index.intValue());
		external.addListener(observer);
		int i = 0;
		beginChange();
		for (E element : external)
			getAddBiConsumer().accept(i++, element);
		endChange();
	}

	public BiConsumer<Integer, E> getAddBiConsumer() {
		return addBiConsumer;
	}

	public Consumer<Integer> getRemoveConsumer() {
		return removeConsumer;
	}

	@Override
	public E get(int index) {
		return backingList.get(index);
	}

	@Override
	public int size() {
		return backingList.size();
	}

	@Override
	protected void doAdd(int index, E element) {
		if (elementObserver != null)
			elementObserver.attachListener(element);
		backingList.add(index, element);
	}

	@Override
	protected E doSet(int index, E element) {
		E removed = backingList.set(index, element);
		if (elementObserver != null) {
			elementObserver.detachListener(removed);
			elementObserver.attachListener(element);
		}
		return removed;
	}

	@Override
	protected E doRemove(int index) {
		E removed = backingList.remove(index);
		if (elementObserver != null)
			elementObserver.detachListener(removed);
		return removed;
	}

	@Override
	public int indexOf(Object o) {
		return backingList.indexOf(o);
	}

	@Override
	public int lastIndexOf(Object o) {
		return backingList.lastIndexOf(o);
	}

	@Override
	public boolean contains(Object o) {
		return backingList.contains(o);
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		return backingList.containsAll(c);
	}

	@Override
	public void clear() {
		if (elementObserver != null) {
			final int sz = size();
			for (int i = 0; i < sz; ++i) {
				elementObserver.detachListener(get(i));
			}
		}
		if (hasListeners()) {
			beginChange();
			nextRemove(0, this);
		}
		backingList.clear();
		++modCount;
		if (hasListeners()) {
			endChange();
		}
	}

	@Override
	public void remove(int fromIndex, int toIndex) {
		beginChange();
		for (int i = fromIndex; i < toIndex; ++i) {
			remove(fromIndex);
		}
		endChange();
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		beginChange();
		BitSet bs = new BitSet(c.size());
		for (int i = 0; i < size(); ++i) {
			if (c.contains(get(i))) {
				bs.set(i);
			}
		}
		if (!bs.isEmpty()) {
			int cur = size();
			while ((cur = bs.previousSetBit(cur - 1)) >= 0) {
				remove(cur);
			}
		}
		endChange();
		return !bs.isEmpty();
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		beginChange();
		BitSet bs = new BitSet(c.size());
		for (int i = 0; i < size(); ++i) {
			if (!c.contains(get(i))) {
				bs.set(i);
			}
		}
		if (!bs.isEmpty()) {
			int cur = size();
			while ((cur = bs.previousSetBit(cur - 1)) >= 0) {
				remove(cur);
			}
		}
		endChange();
		return !bs.isEmpty();
	}

	private SortHelper helper;

	@Override
	@SuppressWarnings("unchecked")
	public void sort() {
		if (backingList.isEmpty()) {
			return;
		}
		int[] perm = getSortHelper().sort((List<? extends Comparable>) backingList);
		fireChange(new SimplePermutationChange<>(0, size(), perm, this));
	}

	@Override
	public void sort(Comparator<? super E> comparator) {
		if (backingList.isEmpty()) {
			return;
		}
		int[] perm = getSortHelper().sort(backingList, comparator);
		fireChange(new SimplePermutationChange<>(0, size(), perm, this));
	}

	private SortHelper getSortHelper() {
		if (helper == null) {
			helper = new SortHelper();
		}
		return helper;
	}

	@Override
	public void onChanged(Change<? extends E> change) {
		while (change.next()) {
			beginChange();
			if (change.wasPermutated()) {
				assert false;// Not tested after
				for (int i = change.getFrom(); i < change.getTo(); i++)
					getRemoveConsumer().accept(change.getFrom());
				int index = change.getFrom();
				for (E source : change.getList().subList(change.getFrom(), change.getTo()))
					getAddBiConsumer().accept(index++, source);
			} else {
				if (change.wasRemoved()) {
					for (int i = 0; i < change.getRemovedSize(); i++)
						getRemoveConsumer().accept(change.getFrom());
				}
				if (change.wasAdded()) {
					int index = change.getFrom();
					for (E source : change.getAddedSubList())
						getAddBiConsumer().accept(index++, source);
				}
			}
			endChange();
		}
	}

	public static class BindingHelperObserver<E> implements ListChangeListener<E> {

		private final WeakReference<ObservableListWrapper<E>> ref;

		public BindingHelperObserver(ObservableListWrapper<E> transformationList) {
			if (transformationList == null) {
				throw new NullPointerException("Binding has to be specified.");
			}
			ref = new WeakReference<>(transformationList);
		}

		@Override
		public void onChanged(Change<? extends E> change) {
			final ObservableListWrapper<E> binding = ref.get();
			if (binding == null) {
				change.getList().removeListener(this);
			} else {
				binding.onChanged(change);
			}

		}

	}

	public static class SortHelper {
		private int[] permutation;
		private int[] reversePermutation;

		private static final int INSERTIONSORT_THRESHOLD = 7;

		public <T extends Comparable<? super T>> int[] sort(List<T> list) {
			T[] a = (T[]) Array.newInstance(Comparable.class, list.size());
			try {
				a = list.toArray(a);
			} catch (ArrayStoreException e) {
				// this means this is not comparable (used without generics)
				throw new ClassCastException();
			}
			int[] result = sort(a);
			ListIterator<T> i = list.listIterator();
			for (int j = 0; j < a.length; j++) {
				i.next();
				i.set(a[j]);
			}
			return result;
		}

		public <T> int[] sort(List<T> list, Comparator<? super T> c) {
			Object[] a = list.toArray();
			int[] result = sort(a, (Comparator) c);
			ListIterator i = list.listIterator();
			for (int j = 0; j < a.length; j++) {
				i.next();
				i.set(a[j]);
			}
			return result;
		}

		public <T extends Comparable<? super T>> int[] sort(T[] a) {
			return sort(a, null);
		}

		public <T> int[] sort(T[] a, Comparator<? super T> c) {
			T[] aux = a.clone();
			int[] result = initPermutation(a.length);
			if (c == null)
				mergeSort(aux, a, 0, a.length, 0);
			else
				mergeSort(aux, a, 0, a.length, 0, c);
			reversePermutation = null;
			permutation = null;
			return result;
		}

		public <T> int[] sort(T[] a, int fromIndex, int toIndex, Comparator<? super T> c) {
			rangeCheck(a.length, fromIndex, toIndex);
			T[] aux = copyOfRange(a, fromIndex, toIndex);
			int[] result = initPermutation(a.length);
			if (c == null)
				mergeSort(aux, a, fromIndex, toIndex, -fromIndex);
			else
				mergeSort(aux, a, fromIndex, toIndex, -fromIndex, c);
			reversePermutation = null;
			permutation = null;
			return Arrays.copyOfRange(result, fromIndex, toIndex);
		}

		public int[] sort(int[] a, int fromIndex, int toIndex) {
			rangeCheck(a.length, fromIndex, toIndex);
			int[] aux = copyOfRange(a, fromIndex, toIndex);
			int[] result = initPermutation(a.length);
			mergeSort(aux, a, fromIndex, toIndex, -fromIndex);
			reversePermutation = null;
			permutation = null;
			return Arrays.copyOfRange(result, fromIndex, toIndex);
		}

		private static void rangeCheck(int arrayLen, int fromIndex, int toIndex) {
			if (fromIndex > toIndex)
				throw new IllegalArgumentException("fromIndex(" + fromIndex + ") > toIndex(" + toIndex + ")");
			if (fromIndex < 0)
				throw new ArrayIndexOutOfBoundsException(fromIndex);
			if (toIndex > arrayLen)
				throw new ArrayIndexOutOfBoundsException(toIndex);
		}

		private static int[] copyOfRange(int[] original, int from, int to) {
			int newLength = to - from;
			if (newLength < 0)
				throw new IllegalArgumentException(from + " > " + to);
			int[] copy = new int[newLength];
			System.arraycopy(original, from, copy, 0, Math.min(original.length - from, newLength));
			return copy;
		}

		private static <T> T[] copyOfRange(T[] original, int from, int to) {
			return copyOfRange(original, from, to, (Class<T[]>) original.getClass());
		}

		private static <T, U> T[] copyOfRange(U[] original, int from, int to, Class<? extends T[]> newType) {
			int newLength = to - from;
			if (newLength < 0)
				throw new IllegalArgumentException(from + " > " + to);
			T[] copy = ((Object) newType == (Object) Object[].class) ? (T[]) new Object[newLength]
					: (T[]) Array.newInstance(newType.getComponentType(), newLength);
			System.arraycopy(original, from, copy, 0, Math.min(original.length - from, newLength));
			return copy;
		}

		/**
		 * Merge sort from Oracle JDK 6
		 */
		private void mergeSort(int[] src, int[] dest, int low, int high, int off) {
			int length = high - low;

			// Insertion sort on smallest arrays
			if (length < INSERTIONSORT_THRESHOLD) {
				for (int i = low; i < high; i++)
					for (int j = i; j > low && ((Comparable) dest[j - 1]).compareTo(dest[j]) > 0; j--)
						swap(dest, j, j - 1);
				return;
			}

			// Recursively sort halves of dest into src
			int destLow = low;
			int destHigh = high;
			low += off;
			high += off;
			int mid = (low + high) >>> 1;
			mergeSort(dest, src, low, mid, -off);
			mergeSort(dest, src, mid, high, -off);

			// If list is already sorted, just copy from src to dest. This is an
			// optimization that results in faster sorts for nearly ordered lists.
			if (((Comparable) src[mid - 1]).compareTo(src[mid]) <= 0) {
				System.arraycopy(src, low, dest, destLow, length);
				return;
			}

			// Merge sorted halves (now in src) into dest
			for (int i = destLow, p = low, q = mid; i < destHigh; i++) {
				if (q >= high || p < mid && ((Comparable) src[p]).compareTo(src[q]) <= 0) {
					dest[i] = src[p];
					permutation[reversePermutation[p++]] = i;
				} else {
					dest[i] = src[q];
					permutation[reversePermutation[q++]] = i;
				}
			}

			for (int i = destLow; i < destHigh; ++i) {
				reversePermutation[permutation[i]] = i;
			}
		}

		/**
		 * Merge sort from Oracle JDK 6
		 */
		private void mergeSort(Object[] src, Object[] dest, int low, int high, int off) {
			int length = high - low;

			// Insertion sort on smallest arrays
			if (length < INSERTIONSORT_THRESHOLD) {
				for (int i = low; i < high; i++)
					for (int j = i; j > low && ((Comparable) dest[j - 1]).compareTo(dest[j]) > 0; j--)
						swap(dest, j, j - 1);
				return;
			}

			// Recursively sort halves of dest into src
			int destLow = low;
			int destHigh = high;
			low += off;
			high += off;
			int mid = (low + high) >>> 1;
			mergeSort(dest, src, low, mid, -off);
			mergeSort(dest, src, mid, high, -off);

			// If list is already sorted, just copy from src to dest. This is an
			// optimization that results in faster sorts for nearly ordered lists.
			if (((Comparable) src[mid - 1]).compareTo(src[mid]) <= 0) {
				System.arraycopy(src, low, dest, destLow, length);
				return;
			}

			// Merge sorted halves (now in src) into dest
			for (int i = destLow, p = low, q = mid; i < destHigh; i++) {
				if (q >= high || p < mid && ((Comparable) src[p]).compareTo(src[q]) <= 0) {
					dest[i] = src[p];
					permutation[reversePermutation[p++]] = i;
				} else {
					dest[i] = src[q];
					permutation[reversePermutation[q++]] = i;
				}
			}

			for (int i = destLow; i < destHigh; ++i) {
				reversePermutation[permutation[i]] = i;
			}
		}

		private void mergeSort(Object[] src, Object[] dest, int low, int high, int off, Comparator c) {
			int length = high - low;

			// Insertion sort on smallest arrays
			if (length < INSERTIONSORT_THRESHOLD) {
				for (int i = low; i < high; i++)
					for (int j = i; j > low && c.compare(dest[j - 1], dest[j]) > 0; j--)
						swap(dest, j, j - 1);
				return;
			}

			// Recursively sort halves of dest into src
			int destLow = low;
			int destHigh = high;
			low += off;
			high += off;
			int mid = (low + high) >>> 1;
			mergeSort(dest, src, low, mid, -off, c);
			mergeSort(dest, src, mid, high, -off, c);

			// If list is already sorted, just copy from src to dest. This is an
			// optimization that results in faster sorts for nearly ordered lists.
			if (c.compare(src[mid - 1], src[mid]) <= 0) {
				System.arraycopy(src, low, dest, destLow, length);
				return;
			}

			// Merge sorted halves (now in src) into dest
			for (int i = destLow, p = low, q = mid; i < destHigh; i++) {
				if (q >= high || p < mid && c.compare(src[p], src[q]) <= 0) {
					dest[i] = src[p];
					permutation[reversePermutation[p++]] = i;
				} else {
					dest[i] = src[q];
					permutation[reversePermutation[q++]] = i;
				}
			}

			for (int i = destLow; i < destHigh; ++i) {
				reversePermutation[permutation[i]] = i;
			}
		}

		private void swap(int[] x, int a, int b) {
			int t = x[a];
			x[a] = x[b];
			x[b] = t;
			permutation[reversePermutation[a]] = b;
			permutation[reversePermutation[b]] = a;
			int tp = reversePermutation[a];
			reversePermutation[a] = reversePermutation[b];
			reversePermutation[b] = tp;
		}

		private void swap(Object[] x, int a, int b) {
			Object t = x[a];
			x[a] = x[b];
			x[b] = t;
			permutation[reversePermutation[a]] = b;
			permutation[reversePermutation[b]] = a;
			int tp = reversePermutation[a];
			reversePermutation[a] = reversePermutation[b];
			reversePermutation[b] = tp;
		}

		private int[] initPermutation(int length) {
			permutation = new int[length];
			reversePermutation = new int[length];
			for (int i = 0; i < length; ++i) {
				permutation[i] = reversePermutation[i] = i;
			}
			return permutation;
		}
	}

}
