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
import org.genericsystem.reactor.annotations.ForEachContext.ForEachContextGenericProcessor;
import org.genericsystem.reactor.annotations.ForEachContext.ForEachContextProcessor;
import org.genericsystem.reactor.annotations.ForEachContext.ForEachContexts;
import org.genericsystem.reactor.context.ObservableListExtractorFromContext;
import org.genericsystem.reactor.extended.ExtendedRootTag.GTag;
import org.genericsystem.reactor.extended.ExtendedRootTag.GTagAnnotation;
import org.genericsystem.reactor.extended.ExtendedRootTag.GTagAnnotationContent;
import org.genericsystem.reactor.gscomponents.TagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(ForEachContexts.class)
@Process(ForEachContextProcessor.class)
@GenericProcess(ForEachContextGenericProcessor.class)
public @interface ForEachContext {

	Class<? extends TagImpl>[] path() default {};

	Class<? extends ObservableListExtractorFromContext> value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface ForEachContexts {
		ForEachContext[] value();
	}

	public static class ForEachContextProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.getRootTag().processForEachContext(tag, ((ForEachContext) annotation).value());
		}
	}

	public static class ForEachContextGenericProcessor implements MetaBindingAnnotationProcessor {

		@Override
		public void setAnnotation(GTag gTag, Annotation annotation) {
			gTag.setAnnotation(ForEachContext.class, null, ((ForEachContext) annotation).value().getName(), ((ForEachContext) annotation).path(), ((ForEachContext) annotation).pos());
		}

		@Override
		public void onAdd(Tag tag, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.getRootTag().processForEachContext(tag, annotationContent.getClassContent());
		}
	}
}
