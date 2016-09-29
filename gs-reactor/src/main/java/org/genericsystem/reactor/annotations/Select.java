package org.genericsystem.reactor.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.Supplier;

import org.genericsystem.reactor.annotations.Select.Selects;
import org.genericsystem.reactor.az.GSTagImpl;
import org.genericsystem.reactor.model.ObservableValueSelector;

/**
 * @author Nicolas Feybesse
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(Selects.class)
public @interface Select {
	Class<? extends GSTagImpl>[] decorate() default {};

	Class<? extends Supplier<ObservableValueSelector>> value();

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Selects {
		Select[] value();
	}

}