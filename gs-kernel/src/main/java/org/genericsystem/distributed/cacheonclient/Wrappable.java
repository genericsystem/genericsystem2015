package org.genericsystem.distributed.cacheonclient;

public interface Wrappable<T> {

	public int size();

	public T get(int index);

}
