package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Select.SelectProcessor;
import org.genericsystem.reactor.annotations.Select.Selects;
import org.genericsystem.reactor.context.ObservableValueSelector;
import org.genericsystem.reactor.gscomponents.TagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(Selects.class)
@Process(SelectProcessor.class)
public @interface Select {
	Class<? extends TagImpl>[] path() default {};

	Class<? extends ObservableValueSelector> value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Selects {
		Select[] value();
	}

	public static class SelectProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.getRootTag().processSelect(tag, ((Select) annotation).value());
		}
	}
}