package org.genericsystem.reactor.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.genericsystem.reactor.annotations.BindText.BindTexts;
import org.genericsystem.reactor.gscomponents.GSTagImpl;
import org.genericsystem.reactor.model.TextBinding;
import org.genericsystem.reactor.model.TextBinding.GENERIC_STRING;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(BindTexts.class)
public @interface BindText {
	Class<? extends GSTagImpl>[] path() default {};

	Class<? extends TextBinding> value() default GENERIC_STRING.class;

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface BindTexts {
		BindText[] value();
	}
}
