package org.genericsystem.api.core;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javafx.util.StringConverter;
import javafx.util.converter.BooleanStringConverter;
import javafx.util.converter.DoubleStringConverter;
import javafx.util.converter.FloatStringConverter;
import javafx.util.converter.IntegerStringConverter;
import javafx.util.converter.LongStringConverter;
import javafx.util.converter.ShortStringConverter;

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
	public final static long TS_OLD_SYSTEM = 1L;
	public final static long[] SYSTEM_TS = new long[] { TS_SYSTEM, 0L, Long.MAX_VALUE };
	public final static long[] USER_TS = new long[] { Long.MAX_VALUE, 0L, Long.MAX_VALUE };

	public final static Map<Integer, Class<?>> SUPPORTED_VALUE_CLASSES = new LinkedHashMap<Integer, Class<?>>() {
		{
			put(0, AxedPropertyClass.class);
			put(1, Boolean.class);
			put(2, byte[].class);
			put(3, Double.class);
			put(4, Float.class);
			put(5, Integer.class);
			put(6, Long.class);
			put(7, Short.class);
			put(8, String.class);
			put(9, Class.class);
		}
	};

	public final static Map<Class<?>, StringConverter> STRING_CONVERTERS = new LinkedHashMap<Class<?>, StringConverter>() {
		{
			put(AxedPropertyClass.class, new StringConverter<AxedPropertyClass>() {

				@Override
				public String toString(AxedPropertyClass object) {
					return object != null ? object.toString() : null;
				}

				@Override
				public AxedPropertyClass fromString(String string) {
					try {
						String[] parts = string.trim().split("#");
						return new AxedPropertyClass(Class.forName(parts[0]), Integer.parseInt(parts[1]));
					} catch (ArrayIndexOutOfBoundsException | ClassNotFoundException | ClassCastException | NumberFormatException e) {
						throw new IllegalStateException();
					}
				}

			});
			put(Boolean.class, new BooleanStringConverter());
			put(byte[].class, new StringConverter<byte[]>() {

				@Override
				public String toString(byte[] bytes) {
					return new String(bytes);
				}

				@Override
				public byte[] fromString(String string) {
					return string.getBytes();
				}

			});
			put(Double.class, new DoubleStringConverter());
			put(Float.class, new FloatStringConverter());
			put(Integer.class, new IntegerStringConverter());
			put(Long.class, new LongStringConverter());
			put(Short.class, new ShortStringConverter());
			put(String.class, new StringConverter<String>() {

				@Override
				public String toString(String string) {
					return string != null && !string.trim().isEmpty() ? string.trim() : null;
				}

				@Override
				public String fromString(String string) {
					return string != null && !string.trim().isEmpty() ? string.trim() : null;
				}

			});
			put(Class.class, new StringConverter<Class<?>>() {

				@Override
				public String toString(Class<?> clazz) {
					return clazz != null ? clazz.getName() : null;
				}

				@Override
				public Class<?> fromString(String className) {
					try {
						return Class.forName(className.trim());
					} catch (ClassNotFoundException e) {
						throw new IllegalStateException();
					}
				}

			});
		}
	};

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
	public static <T extends IGeneric<T>> boolean areOverridesReached(List<T> supers, List<T> overrides) {
		return overrides.stream().allMatch(override -> supers.stream().anyMatch(superVertex -> superVertex.inheritsFrom(override)));
	}

}
