package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.SwitcherSubSteps.SwitcherSubStepsProcessor;
import org.genericsystem.reactor.gscomponents.TagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Process(SwitcherSubStepsProcessor.class)
public @interface SwitcherSubSteps {
	Class<? extends TagImpl>[] path() default {};

	int[] pos() default {};

	int switchClassPos() default 0;

	public static class SwitcherSubStepsProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {

			tag.getMetaBinding().filter(tag);

		}
	}
}
