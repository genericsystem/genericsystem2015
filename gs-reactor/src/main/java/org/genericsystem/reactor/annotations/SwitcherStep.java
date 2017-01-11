package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.Map;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.SwitcherStep.SwitcherStepProcessor;
import org.genericsystem.reactor.gscomponents.SwitchChildDiv.SwitchSubSteps;
import org.genericsystem.reactor.gscomponents.TagImpl;

import javafx.beans.property.Property;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Process(SwitcherStepProcessor.class)
public @interface SwitcherStep {
	Class<? extends TagImpl>[] path() default {};

	int[] pos() default {};

	int switchClassPos() default 0;

	public static class SwitcherStepProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.addPrefixBinding(context -> {
				Property<Map<Tag, SwitchSubSteps>> property = tag.<Map<Tag, SwitchSubSteps>> getProperty(Switcher.SWITCHER_MAP, context);
				Map<Tag, SwitchSubSteps> map = property.getValue();
				SwitchSubSteps subSteps = map.get(tag);
				if (subSteps == null)
					map.put(tag, subSteps = new SwitchSubSteps(tag.<Class<? extends TagImpl>> getProperty(Switcher.SELECTED_CLASS, context)));
			});
		}
	}
}
