package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.genericsystem.reactor.annotations.AddCustomAnnotation.AddCustomAnnotations;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(AddCustomAnnotations.class)
public @interface AddCustomAnnotation {

	Class<? extends Annotation>[] value();

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface AddCustomAnnotations {
		AddCustomAnnotation[] value();
	}
}
