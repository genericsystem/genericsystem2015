package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Mode.ModeProcessor;
import org.genericsystem.reactor.annotations.Mode.Modes;
import org.genericsystem.reactor.gscomponents.GSTagImpl;
import org.genericsystem.reactor.model.TagSelector;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(Modes.class)
@Process(ModeProcessor.class)
public @interface Mode {
	Class<? extends GSTagImpl>[] path() default {};

	int[] pos() default {};

	Class<? extends TagSelector> value();

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Modes {
		Mode[] value();
	}

	public static class ModeProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			try {
				tag.setTagSelector(((Mode) annotation).value().newInstance());
			} catch (IllegalAccessException | InstantiationException e) {
				throw new IllegalStateException(e);
			}
		}
	}
}
