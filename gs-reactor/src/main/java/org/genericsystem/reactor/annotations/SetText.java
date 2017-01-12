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
import org.genericsystem.reactor.annotations.SetText.SetTextGenericProcessor;
import org.genericsystem.reactor.annotations.SetText.SetTextProcessor;
import org.genericsystem.reactor.annotations.SetText.SetTexts;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTag;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotation;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotationContent;
import org.genericsystem.reactor.gscomponents.TagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(SetTexts.class)
@Process(SetTextProcessor.class)
@GenericProcess(SetTextGenericProcessor.class)
public @interface SetText {
	Class<? extends TagImpl>[] path() default {};

	String[] value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface SetTexts {
		SetText[] value();
	}

	public static class SetTextProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.getRootTag().processSetText(tag, ((SetText) annotation).path(), ((SetText) annotation).value());
		}
	}

	public static class SetTextGenericProcessor implements IGenericAnnotationProcessor {

		@Override
		public void setAnnotation(GTag gTag, Annotation annotation) {
			gTag.setArrayValueAnnotation(SetText.class, null, ((SetText) annotation).value(), ((SetText) annotation).path(), ((SetText) annotation).pos());
		}

		@Override
		public void onRemove(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.setText(context, "");
		}

		@Override
		public void onAdd(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.getRootTag().processSetText(tag, context, gTagAnnotation.getValue().getPath(), annotationContent.getStringArrayContent());
		}
	}
}
