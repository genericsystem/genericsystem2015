package org.genericsystem.reactor.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.genericsystem.reactor.annotations.ReactorDependencies.ReactorDependenciesMult;
import org.genericsystem.reactor.az.GSTagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(ReactorDependenciesMult.class)
public @interface ReactorDependencies {
	Class<? extends GSTagImpl>[] path() default {};

	Class<? extends GSTagImpl>[] value();

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface ReactorDependenciesMult {
		ReactorDependencies[] value();
	}
}
