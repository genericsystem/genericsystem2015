package org.genericsystem.api.core;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 *            the implementation of IVertex used for all nodes.
 */
public interface IContext<T extends IGeneric<T>> {

	Snapshot<T> getAttributes(T generic, T attribute);

	Snapshot<T> getHolders(T generic, T attribute);
}
