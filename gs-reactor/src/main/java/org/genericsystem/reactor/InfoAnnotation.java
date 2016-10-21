package org.genericsystem.reactor;

import java.lang.annotation.Annotation;
import java.util.function.BiConsumer;

public class InfoAnnotation {

	Class<? extends Annotation> annotationClass;
	BiConsumer<Annotation, Tag> consumer;
	boolean isRepeatable;

	public InfoAnnotation(Class<? extends Annotation> annotationClass, BiConsumer<Annotation, Tag> consumer, boolean isRepeatable) {
		this.annotationClass = annotationClass;
		this.consumer = consumer;
		this.isRepeatable = isRepeatable;
	}

	public Class<? extends Annotation> getAnnotationClass() {
		return annotationClass;
	}

	public BiConsumer<Annotation, Tag> getConsumer() {
		return consumer;
	}

	public boolean isRepeatable() {
		return isRepeatable;
	}
}
