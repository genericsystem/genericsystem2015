package org.genericsystem.reactor.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.genericsystem.reactor.annotations.SetStringExtractor.SetStringExtractors;
import org.genericsystem.reactor.gscomponents.GSTagImpl;
import org.genericsystem.reactor.model.StringExtractor;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(SetStringExtractors.class)
public @interface SetStringExtractor {
	Class<? extends GSTagImpl>[] path() default {};

	Class<? extends StringExtractor> value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface SetStringExtractors {
		SetStringExtractor[] value();
	}
}
