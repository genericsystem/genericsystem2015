package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.api.core.TagAnnotation;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.ExtendedAnnotationsManager.IGenericAnnotationProcessor;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindSelection.BindSelectionGenericProcessor;
import org.genericsystem.reactor.annotations.BindSelection.BindSelectionProcessor;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTag;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotation;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotationContent;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.TagType.TagAnnotationAttribute;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.TagType.TagAnnotationContentAttribute;
import org.genericsystem.reactor.gscomponents.TagImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.json.JsonObject;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Process(BindSelectionProcessor.class)
@GenericProcess(BindSelectionGenericProcessor.class)
public @interface BindSelection {
	Class<? extends TagImpl>[] path() default {};

	Class<? extends TagImpl> value();

	int[] pos() default {};

	int valuePos() default 0;

	public static class BindSelectionProcessor implements BiConsumer<Annotation, Tag> {
		private static final Logger log = LoggerFactory.getLogger(BindSelectionProcessor.class);

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.getRootTag().processBindSelection(tag, ((BindSelection) annotation).value(), ((BindSelection) annotation).valuePos());
		}
	}

	public static class BindSelectionGenericProcessor implements IGenericAnnotationProcessor {

		@Override
		public void setAnnotation(GTag gTag, Annotation annotation) {
			BindSelection annotation_ = (BindSelection) annotation;
			GTagAnnotation gTagAnnotation = (GTagAnnotation) gTag.setHolder(gTag.getRoot().find(TagAnnotationAttribute.class), new TagAnnotation(BindSelection.class, annotation_.path(), annotation_.pos()));
			gTagAnnotation.setHolder(gTag.getRoot().find(TagAnnotationContentAttribute.class), new JsonObject().put("value", annotation_.value().getName()).put("valuePos", annotation_.valuePos()).encodePrettily());
		}

		@Override
		public void onRemove(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			((SelectionDefaults) tag).unbindSelection(tag.find((Class<? extends TagImpl>) annotationContent.getClassContent(), annotationContent.getJsonValue().getInteger("valuePos").intValue()), context);
		}

		@Override
		public void onAdd(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.getRootTag().processBindSelection(tag, context, (Class<? extends TagImpl>) annotationContent.getClassContent(), annotationContent.getJsonValue().getInteger("valuePos").intValue());
		}

		@Override
		public void onRemove(Tag tag, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.addPostfixBinding(context -> onRemove(tag, context, gTagAnnotation, annotationContent));
		}

		@Override
		public void onAdd(Tag tag, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.addPostfixBinding(context -> onAdd(tag, context, gTagAnnotation, annotationContent));
		}
	}
}
