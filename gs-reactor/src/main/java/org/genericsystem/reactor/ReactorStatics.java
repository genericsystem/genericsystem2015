package org.genericsystem.reactor;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.lang.annotation.Annotation;
import java.util.Arrays;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.genericsystem.api.core.AxedPropertyClass;
import org.genericsystem.api.core.TagAnnotation;

import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import javafx.util.StringConverter;
import javafx.util.converter.BooleanStringConverter;
import javafx.util.converter.DoubleStringConverter;
import javafx.util.converter.FloatStringConverter;
import javafx.util.converter.IntegerStringConverter;
import javafx.util.converter.LongStringConverter;
import javafx.util.converter.ShortStringConverter;

public class ReactorStatics {

	public static final String BACKGROUND = "background";
	public static final String CHECKED = "checked";
	public static final String DISABLED = "disabled";

	public static <T> T fromString(String s) {
		byte[] data = Base64.getDecoder().decode(s);
		ObjectInputStream ois;
		try {
			ois = new ObjectInputStream(new ByteArrayInputStream(data));
			Object o = ois.readObject();
			ois.close();
			return (T) o;
		} catch (IOException | ClassNotFoundException e) {
			throw new IllegalStateException(e);
		}
	}

	public static String toString(Serializable o) {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		ObjectOutputStream oos;
		try {
			oos = new ObjectOutputStream(baos);
			oos.writeObject(o);
			oos.close();
		} catch (IOException e) {
			return null;
		}
		return Base64.getEncoder().encodeToString(baos.toByteArray());
	}

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
			put(TagAnnotation.class, new StringConverter<TagAnnotation>() {

				@Override
				public String toString(TagAnnotation object) {
					JsonObject result = new JsonObject();
					result.put("annotationClass", object.getAnnotationClass().getName());
					result.put("path", new JsonArray(Arrays.stream(object.getPath()).map(clazz -> clazz.getName()).collect(Collectors.toList())));
					result.put("pos", new JsonArray(IntStream.of(object.getPos()).boxed().collect(Collectors.toList())));
					result.put("name", object.getName());
					return result.encodePrettily();
				}

				@Override
				public TagAnnotation fromString(String string) {
					JsonObject json = new JsonObject(string);
					Class<? extends Annotation> annotationClass = null;
					try {
						annotationClass = (Class<? extends Annotation>) Class.forName(json.getString("annotationClass"));
					} catch (ClassNotFoundException e) {
						throw new IllegalStateException("Class " + json.getString("annotationClass") + " not found");
					}
					Class<?>[] path = json.getJsonArray("path").stream().map(className -> {
						try {
							return Class.forName((String) className);
						} catch (ClassNotFoundException e) {
							throw new IllegalStateException("Class " + className + " not found");
						}
					}).toArray(Class<?>[]::new);
					int[] pos = json.getJsonArray("pos").stream().mapToInt(i -> (int) i).toArray();
					String name = json.getString("name");
					return new TagAnnotation(annotationClass, path, pos, name);
				}
			});
		}
	};
}
