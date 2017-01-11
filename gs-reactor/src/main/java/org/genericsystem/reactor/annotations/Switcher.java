package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.HashMap;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Switcher.SwicherProcessor;
import org.genericsystem.reactor.gscomponents.SwitchChildDiv.SwitchSubSteps;
import org.genericsystem.reactor.gscomponents.TagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Process(SwicherProcessor.class)
public @interface Switcher {
	public static final String SWITCHER_MAP = "switcherMap";
	public static final String SELECTED_CLASS = "selectedClass";

	Class<? extends TagImpl> value();

	Class<? extends TagImpl>[] path() default {};

	int[] pos() default {};

	public static class SwicherProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.createNewInitializedProperty(SELECTED_CLASS, context -> ((Switcher) annotation).value());
			tag.createNewInitializedProperty(SWITCHER_MAP, context -> new HashMap<Tag, SwitchSubSteps>());
		}
	}
}
