package org.genericsystem.reactor.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.genericsystem.reactor.gscomponents.GSTagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
public @interface BindSelection {
	Class<? extends GSTagImpl>[] path() default {};

	Class<? extends GSTagImpl> value();

	int[] pos() default {};

	int valuePos() default 0;
}
