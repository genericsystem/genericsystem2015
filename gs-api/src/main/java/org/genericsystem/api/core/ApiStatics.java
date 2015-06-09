package org.genericsystem.api.core;

import java.util.List;

/**
 * Statics constants and methods useful when dealing with Generics.
 * 
 * @author Nicolas Feybesse
 */
public class ApiStatics {
	/**
	 * Constants indicating position of a Generic in a relation.
	 * <p>
	 * Indicates no position for the Generic.
	 * </p>
	 */
	public static final int NO_POSITION = -1;
	/**
	 * Indicates the position for the base of the Generic.
	 */
	public static final int BASE_POSITION = 0;
	/**
	 * Indicates the position for the first target of the Generic.
	 */
	public static final int TARGET_POSITION = 1;
	/**
	 * Indicates the position for the second target of the Generic.
	 */
	public static final int TERNARY_POSITION = 2;

	/**
	 * Constants indicating level of a Generic.
	 * <p>
	 * Indicates the level which corresponds to meta data.
	 * </p>
	 */
	public static final int META = 0;
	/**
	 * Indicates the level which corresponds to types, attributes and relations.
	 */
	public static final int STRUCTURAL = 1;
	/**
	 * Indicates the level which corresponds to instances, holders and links.
	 */
	public static final int CONCRETE = 2;
	/**
	 * Indicates the level which corresponds to feelings. Feelings are data which are released by the concretes : for example, the mood of a person, its emotions and so on.
	 */
	public static final int SENSOR = 3;

	public static final int TYPE_SIZE = 0;
	public static final int ATTRIBUTE_SIZE = 1;
	public static final int RELATION_SIZE = 2;
	public static final int TERNARY_RELATION_SIZE = 3;

	public static final long TS_SYSTEM = 0L;

	/**
	 * Checks that each of the <code>overrides</code> is inherited by at least one of the <code>supers</code>.
	 * 
	 * @param <T>
	 *            the implementation of IVertex used for all nodes.
	 * @param supers
	 *            the Generics that may inherit.
	 * @param overrides
	 *            the Generics that may be inherited.
	 * @return <code>true</code> if each of the <code>overrides</code> is inherited by at least one of the <code>supers</code>, <code>false</code> otherwise.
	 */
	public static <T extends IVertex<T>> boolean areOverridesReached(List<T> supers, List<T> overrides) {
		return overrides.stream().allMatch(override -> supers.stream().anyMatch(superVertex -> superVertex.inheritsFrom(override)));
	}
}
