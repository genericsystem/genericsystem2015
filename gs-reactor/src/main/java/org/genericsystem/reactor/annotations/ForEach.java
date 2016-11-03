package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.ForEach.ForEachProcessor;
import org.genericsystem.reactor.annotations.ForEach.ForEachs;
import org.genericsystem.reactor.gscomponents.TagImpl;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor.NO_FOR_EACH;

/**
 * @author Nicolas Feybesse
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(ForEachs.class)
@Process(ForEachProcessor.class)
public @interface ForEach {
	Class<? extends TagImpl>[] path() default {};

	Class<? extends ObservableListExtractor> value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface ForEachs {
		ForEach[] value();
	}

	public static class ForEachProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			try {
				if (!NO_FOR_EACH.class.equals(((ForEach) annotation).value()))
					tag.forEach(((ForEach) annotation).value().newInstance());
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		}
	}
}