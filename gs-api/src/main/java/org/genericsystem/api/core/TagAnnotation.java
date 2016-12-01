package org.genericsystem.api.core;

import java.io.Serializable;
import java.lang.annotation.Annotation;
import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Collectors;

public class TagAnnotation implements Serializable {

	private static final long serialVersionUID = -2849507333202498923L;

	private Class<? extends Annotation> annotationClass;
	private Class<?>[] path;
	private int[] pos;
	private String name;

	public TagAnnotation(Class<? extends Annotation> annotationClass, Class<?>[] path, int[] pos) {
		this(annotationClass, path, pos, null);
	}

	public TagAnnotation(Class<? extends Annotation> annotationClass, Class<?>[] path, int[] pos, String name) {
		this.annotationClass = annotationClass;
		this.path = path;
		this.pos = pos;
		this.name = name;
	}

	public Class<? extends Annotation> getAnnotationClass() {
		return annotationClass;
	}

	public Class<?>[] getPath() {
		return path;
	}

	public int[] getPos() {
		return pos;
	}

	public String getName() {
		return name;
	}

	@Override
	public int hashCode() {
		return annotationClass.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof TagAnnotation))
			return false;
		TagAnnotation other = (TagAnnotation) obj;
		return annotationClass.equals(other.annotationClass) && Objects.equals(name, other.name) && Objects.deepEquals(path, other.path) && Objects.deepEquals(pos, other.pos);
	}

	@Override
	public String toString() {
		return "{ annotationClass: " + annotationClass.getSimpleName() + ", path: " + Arrays.toString(path) + ", pos: " + Arrays.stream(pos).boxed().collect(Collectors.toList()) + ", name: " + name + "}";
	}
}
