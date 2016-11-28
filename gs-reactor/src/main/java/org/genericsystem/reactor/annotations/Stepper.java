package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindSelection.BindSelectionProcessor;
import org.genericsystem.reactor.annotations.Stepper.StepperProcessor;
import org.genericsystem.reactor.annotations.Stepper.Steppers;
import org.genericsystem.reactor.contextproperties.StepperDefaults;
import org.genericsystem.reactor.gscomponents.TagImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(Steppers.class)
@Process(StepperProcessor.class)
public @interface Stepper {
	Class<? extends TagImpl>[] path() default {};

	int[] pos() default {};

	Class<? extends TagImpl> switchClass();

	int switchClassPos() default 0;

	Class<? extends TagImpl> headerClass();

	int headerClassPos() default 0;

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Steppers {
		Stepper[] value();
	}

	public static class StepperProcessor implements BiConsumer<Annotation, Tag> {
		private static final Logger log = LoggerFactory.getLogger(BindSelectionProcessor.class);

		@Override
		public void accept(Annotation annotation, Tag tag) {
			if (StepperDefaults.class.isAssignableFrom(tag.getClass())) {
				Stepper stepperAnn = (Stepper) annotation;
				((StepperDefaults) tag).stepper(tag.find(stepperAnn.switchClass(), stepperAnn.switchClassPos()), tag.find(stepperAnn.headerClass(), stepperAnn.headerClassPos()));
			} else
				log.warn("Switch is applicable only to tags implementing SwitchDefaults.");

		}
	}
}
