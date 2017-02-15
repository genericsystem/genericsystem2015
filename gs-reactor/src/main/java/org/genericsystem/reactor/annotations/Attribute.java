package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Attribute.AttributeGenericProcessor;
import org.genericsystem.reactor.annotations.Attribute.AttributeProcessor;
import org.genericsystem.reactor.annotations.Attribute.Attributes;
import org.genericsystem.reactor.extended.ExtendedAnnotationsManager.IGenericAnnotationProcessor;
import org.genericsystem.reactor.extended.ExtendedRootTag.GTag;
import org.genericsystem.reactor.extended.ExtendedRootTag.GTagAnnotation;
import org.genericsystem.reactor.extended.ExtendedRootTag.GTagAnnotationContent;
import org.genericsystem.reactor.gscomponents.TagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(Attributes.class)
@Process(value = AttributeProcessor.class, repeatable = true)
@GenericProcess(AttributeGenericProcessor.class)
public @interface Attribute {
	Class<? extends TagImpl>[] path() default {};

	String name();

	String value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Attributes {
		Attribute[] value();
	}

	public static class AttributeProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.getRootTag().processAttribute(tag, ((Attribute) annotation).name(), ((Attribute) annotation).value());
		}
	}

	public static class AttributeGenericProcessor implements IGenericAnnotationProcessor {

		@Override
		public void setAnnotation(GTag gTag, Annotation annotation) {
			Attribute annotation_ = (Attribute) annotation;
			gTag.setAnnotation(Attribute.class, annotation_.name(), annotation_.value(), annotation_.path(), annotation_.pos());
		}

		@Override
		public void onRemove(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.getDomNodeAttributes(context).remove(gTagAnnotation.getValue().getName());
		}

		@Override
		public void onAdd(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.getRootTag().processAttribute(tag, context, gTagAnnotation.getValue().getName(), annotationContent.getContentValue());
		}
	}
}
