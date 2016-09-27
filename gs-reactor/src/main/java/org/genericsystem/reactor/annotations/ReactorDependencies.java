package org.genericsystem.reactor.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.genericsystem.reactor.az.GSTagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Inherited
public @interface ReactorDependencies {
	Class<? extends GSTagImpl>[] value();

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ParentReactorDependencies {
		int pos() default 0;
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildReactorDependencies {
		Class<? extends GSTagImpl>[] value();
	}

}
