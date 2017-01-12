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
import org.genericsystem.reactor.annotations.BindText.BindTextGenericProcessor;
import org.genericsystem.reactor.annotations.BindText.BindTextProcessor;
import org.genericsystem.reactor.annotations.BindText.BindTexts;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.context.TextBinding.GENERIC_STRING;
import org.genericsystem.reactor.contextproperties.TextPropertyDefaults;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTag;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotation;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotationContent;
import org.genericsystem.reactor.gscomponents.TagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(BindTexts.class)
@Process(BindTextProcessor.class)
@GenericProcess(BindTextGenericProcessor.class)
public @interface BindText {
	Class<? extends TagImpl>[] path() default {};

	Class<? extends TextBinding> value() default GENERIC_STRING.class;

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface BindTexts {
		BindText[] value();
	}

	public static class BindTextProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.getRootTag().processBindText(tag, ((BindText) annotation).value());
		}
	}

	public static class BindTextGenericProcessor implements IGenericAnnotationProcessor {

		@Override
		public void setAnnotation(GTag gTag, Annotation annotation) {
			gTag.setAnnotation(BindText.class, null, ((BindText) annotation).value().getName(), ((BindText) annotation).path(), ((BindText) annotation).pos());
		}

		@Override
		public void onRemove(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.getDomNodeTextProperty(context).unbind();
			context.getPropertiesMaps(tag).remove(TextPropertyDefaults.TEXT_BINDING);
			tag.getDomNodeTextProperty(context).setValue(null);
		}

		@Override
		public void onAdd(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.getRootTag().processBindText(tag, context, (Class<? extends TextBinding>) annotationContent.getClassContent());
		}
	}
}
