package org.genericsystem.defaults.tools;

import java.util.Comparator;
import java.util.List;

public interface SortableList<E> extends List<E> {

	/**
	 * Sort using default comparator
	 * 
	 * @throws ClassCastException
	 *             if some of the elements cannot be cast to Comparable
	 * @throws UnsupportedOperationException
	 *             if list's iterator doesn't support set
	 */
	public void sort();

	/**
	 * Sort using comparator
	 * 
	 * @param comparator
	 *            the comparator to use
	 * @throws ClassCastException
	 *             if the list contains elements that are not <i>mutually comparable</i> using the specified comparator.
	 * @throws UnsupportedOperationException
	 *             if the specified list's list-iterator does not support the <tt>set</tt> operation.
	 */
	@Override
	public void sort(Comparator<? super E> comparator);

}