package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.DirectSelect.MetaBindingAnnotationProcessor;
import org.genericsystem.reactor.annotations.ForEach.ForEachGenericProcessor;
import org.genericsystem.reactor.annotations.ForEach.ForEachProcessor;
import org.genericsystem.reactor.annotations.ForEach.ForEachs;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.extended.ExtendedRootTag.GTag;
import org.genericsystem.reactor.extended.ExtendedRootTag.GTagAnnotation;
import org.genericsystem.reactor.extended.ExtendedRootTag.GTagAnnotationContent;
import org.genericsystem.reactor.gscomponents.TagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(ForEachs.class)
@Process(ForEachProcessor.class)
@GenericProcess(ForEachGenericProcessor.class)
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
			tag.getRootTag().processForEach(tag, ((ForEach) annotation).value());
		}
	}

	public static class ForEachGenericProcessor implements MetaBindingAnnotationProcessor {

		@Override
		public void setAnnotation(GTag gTag, Annotation annotation) {
			gTag.setAnnotation(ForEach.class, null, ((ForEach) annotation).value().getName(), ((ForEach) annotation).path(), ((ForEach) annotation).pos());
		}

		@Override
		public void onAdd(Tag tag, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.getRootTag().processForEach(tag, annotationContent.getClassContent());
		}
	}
}