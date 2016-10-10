package org.genericsystem.reactor.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.genericsystem.reactor.annotations.Stepper.Steppers;
import org.genericsystem.reactor.gscomponents.GSTagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(Steppers.class)
public @interface Stepper {
	Class<? extends GSTagImpl>[] path() default {};

	Class<? extends GSTagImpl> switchClass();

	Class<? extends GSTagImpl> headerClass();

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Steppers {
		Stepper[] value();
	}
}
