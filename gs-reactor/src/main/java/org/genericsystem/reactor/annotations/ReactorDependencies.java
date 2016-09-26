package org.genericsystem.reactor.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.genericsystem.reactor.gs.GSTagImpl;
import org.genericsystem.reactor.gs3.GSComposite.GSContentComponent;

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
		int value() default 0;
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	@Repeatable(ChildReactorDependenciesMult.class)
	public @interface ChildReactorDependencies {
		Class<? extends GSTagImpl> decorate() default GSContentComponent.class;

		Class<? extends GSTagImpl> value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildReactorDependenciesMult {
		ChildReactorDependencies[] value();
	}

}
