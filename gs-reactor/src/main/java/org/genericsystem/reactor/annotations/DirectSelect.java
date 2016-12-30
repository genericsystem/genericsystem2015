package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.DirectSelect.DirectSelects;
import org.genericsystem.reactor.annotations.DirectSelect.Processor;
import org.genericsystem.reactor.gscomponents.TagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(DirectSelects.class)
@Process(Processor.class)
public @interface DirectSelect {
	Class<? extends TagImpl>[] path() default {};

	Class<?>[] value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface DirectSelects {
		DirectSelect[] value();
	}

	public static class Processor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.getRootTag().processDirectSelect(tag, ((DirectSelect) annotation).path(), ((DirectSelect) annotation).value());
		}
	}
}