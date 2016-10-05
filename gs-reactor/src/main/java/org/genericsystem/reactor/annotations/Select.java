package org.genericsystem.reactor.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.genericsystem.reactor.annotations.Select.Selects;
import org.genericsystem.reactor.ca_gscomponents.GSTagImpl;
import org.genericsystem.reactor.model.ObservableModelSelector;
import org.genericsystem.reactor.model.ObservableValueSelector;

/**
 * @author Nicolas Feybesse
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(Selects.class)
public @interface Select {
	Class<? extends GSTagImpl>[] path() default {};

	Class<? extends ObservableValueSelector> value();

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Selects {
		Select[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(SelectModels.class)
	public @interface SelectModel {
		Class<? extends GSTagImpl>[] path() default {};

		Class<? extends ObservableModelSelector> value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface SelectModels {
		SelectModel[] value();
	}
}