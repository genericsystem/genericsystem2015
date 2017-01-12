package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Switcher.SwicherProcessor;
import org.genericsystem.reactor.gscomponents.Controller;
import org.genericsystem.reactor.gscomponents.TagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Process(SwicherProcessor.class)
public @interface Switcher {
	public static final String CONTROLLER = "controller";

	Class<? extends TagImpl> value();

	Class<? extends TagImpl>[] path() default {};

	int[] pos() default {};

	public static class SwicherProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.createNewInitializedProperty(CONTROLLER, context -> new Controller(((Switcher) annotation).value()));
		}
	}
}
