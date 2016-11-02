package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Switch.SwichProcessor;
import org.genericsystem.reactor.annotations.Switch.Modes;
import org.genericsystem.reactor.gscomponents.GSTagImpl;
import org.genericsystem.reactor.model.TagSwitcher;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(Modes.class)
@Process(SwichProcessor.class)
public @interface Switch {
	Class<? extends GSTagImpl>[] path() default {};

	int[] pos() default {};

	Class<? extends TagSwitcher> value();

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Modes {
		Switch[] value();
	}

	public static class SwichProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			try {
				tag.setSwitcher(((Switch) annotation).value().newInstance());
			} catch (IllegalAccessException | InstantiationException e) {
				throw new IllegalStateException(e);
			}
		}
	}
}
