package org.genericsystem.reactor.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.Supplier;

import org.genericsystem.reactor.az.GSTagImpl;
import org.genericsystem.reactor.model.ObservableValueSelector;

/**
 * @author Nicolas Feybesse
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Inherited
public @interface Select {
	Class<? extends Supplier<ObservableValueSelector>> value();

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	@Repeatable(ChildSelects.class)
	public @interface ChildSelect {
		Class<? extends GSTagImpl> decorate();

		Class<? extends Supplier<ObservableValueSelector>> value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildSelects {
		ChildSelect[] value();
	}

}