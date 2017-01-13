package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Stepper.StepperProcessor;
import org.genericsystem.reactor.gscomponents.Controller;
import org.genericsystem.reactor.gscomponents.TagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Process(StepperProcessor.class)
public @interface Stepper {

	Class<? extends TagImpl> first();

	Class<? extends TagImpl>[] path() default {};

	int[] pos() default {};

	public static class StepperProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			Controller.initialize(tag, ((Stepper) annotation).first());
		}
	}
}
