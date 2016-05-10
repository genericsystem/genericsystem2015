package org.genericsystem.api.core;

/**
 * Represents the root of Generic System.
 * 
 * @author Nicolas Feybesse
 *
 * @param <T>
 *            the implementation of IRoot used for engine.
 */
public interface IRoot<T extends IVertex<T>> extends IVertex<T> {

	/**
	 * Return a vertex built during new Root.
	 *
	 * @param <Custom>
	 *            an implementation of a customizable subtype of T.
	 * @param clazz
	 *            the expected vertex.
	 * @return a vertex.
	 */
	<Custom extends T> Custom find(Class<?> clazz);
	
	/**
	 * Return a vertex built during new Root.
	 * If called during Root initialization, mount system node if nesscessary
	 * 
	 * @param <Custom>
	 *            an implementation of a customizable subtype of T.
	 * @param clazz
	 *            the expected vertex.
	 * @return a vertex.
	 */
	<Custom extends T> Custom bind(Class<?> clazz);

	Class<?> findAnnotedClass(T vertex);

	/*
	 * T addType(Serializable value);
	 * 
	 * T addType(T override, Serializable value);
	 * 
	 * T addType(List<T> overrides, Serializable value);
	 * 
	 * T setType(Serializable value);
	 * 
	 * T setType(T override, Serializable value);
	 * 
	 * T setType(List<T> overrides, Serializable value);
	 */

	/**
	 * Return the meta attribute. The meta attribute is the super of all attributes.
	 * 
	 * @return the meta attribute.
	 */
	T getMetaAttribute();

	/**
	 * Return the meta relation. The meta relation is the super of all relations.
	 * 
	 * @return the meta relation.
	 */
	T getMetaRelation();

	/**
	 * Close the root. All changes done in the cache but not committed are automatically rollbacked. Persist the last state of the engine.
	 */
	void close();

	T getMap();
}
