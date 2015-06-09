package org.genericsystem.kernel.iterator;

/**
 * @author Nicolas Feybesse
 * 
 * @param <T>
 */
public abstract class AbstractAwareIterator<T> extends AbstractGeneralAwareIterator<T, T> {

	@Override
	public T project() {
		return next;
	}

}
