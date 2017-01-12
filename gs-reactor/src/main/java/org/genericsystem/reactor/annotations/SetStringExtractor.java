package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.ExtendedAnnotationsManager.IGenericAnnotationProcessor;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.SetStringExtractor.SetStringExtractorGenericProcessor;
import org.genericsystem.reactor.annotations.SetStringExtractor.SetStringExtractorProcessor;
import org.genericsystem.reactor.annotations.SetStringExtractor.SetStringExtractors;
import org.genericsystem.reactor.context.StringExtractor;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTag;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotation;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotationContent;
import org.genericsystem.reactor.gscomponents.TagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(SetStringExtractors.class)
@Process(SetStringExtractorProcessor.class)
@GenericProcess(SetStringExtractorGenericProcessor.class)
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
			tag.getRootTag().processSetStringExtractor(tag, ((SetStringExtractor) annotation).value());
		}
	}

	public static class SetStringExtractorGenericProcessor implements IGenericAnnotationProcessor {

		@Override
		public void setAnnotation(GTag gTag, Annotation annotation) {
			SetStringExtractor annotation_ = (SetStringExtractor) annotation;
			gTag.setAnnotation(SetStringExtractor.class, null, annotation_.value().getName(), annotation_.path(), annotation_.pos());
		}

		@Override
		public void onRemove(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.getStringExtractorProperty(context).setValue(null);
		}

		@Override
		public void onAdd(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.getRootTag().processSetStringExtractor(tag, context, (Class<? extends StringExtractor>) annotationContent.getClassContent());
		}
	}
}
