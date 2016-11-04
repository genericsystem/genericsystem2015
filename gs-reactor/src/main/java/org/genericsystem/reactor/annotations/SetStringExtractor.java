package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.SetStringExtractor.SetStringExtractorProcessor;
import org.genericsystem.reactor.annotations.SetStringExtractor.SetStringExtractors;
import org.genericsystem.reactor.gscomponents.TagImpl;
import org.genericsystem.reactor.model.StringExtractor;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(SetStringExtractors.class)
@Process(SetStringExtractorProcessor.class)
public @interface SetStringExtractor {
	Class<? extends TagImpl>[] path() default {};

	Class<? extends StringExtractor> value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface SetStringExtractors {
		SetStringExtractor[] value();
	}

	public static class SetStringExtractorProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			try {
				tag.setStringExtractor(((SetStringExtractor) annotation).value().newInstance());
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		}
	}
}
