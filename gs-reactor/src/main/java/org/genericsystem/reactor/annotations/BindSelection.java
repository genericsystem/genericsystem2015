package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindSelection.BindSelectionProcessor;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.TagImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Process(BindSelectionProcessor.class)
public @interface BindSelection {
	Class<? extends TagImpl>[] path() default {};

	Class<? extends TagImpl> value();

	int[] pos() default {};

	int valuePos() default 0;

	public static class BindSelectionProcessor implements BiConsumer<Annotation, Tag> {
		private static final Logger log = LoggerFactory.getLogger(BindSelectionProcessor.class);

		@Override
		public void accept(Annotation annotation, Tag tag) {
			if (SelectionDefaults.class.isAssignableFrom(tag.getClass()))
				((SelectionDefaults) tag).bindSelection(tag.find(((BindSelection) annotation).value(), ((BindSelection) annotation).valuePos()));
			else
				log.warn("BindSelection is applicable only to a class implementing SelectionDefaults.");

		}
	}
}
