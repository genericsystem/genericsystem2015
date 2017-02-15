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
import org.genericsystem.reactor.annotations.StyleClass.StyleClassGenericProcessor;
import org.genericsystem.reactor.annotations.StyleClass.StyleClassProcessor;
import org.genericsystem.reactor.annotations.StyleClass.StyleClasses;
import org.genericsystem.reactor.extended.ExtendedAnnotationsManager.IGenericAnnotationProcessor;
import org.genericsystem.reactor.extended.ExtendedRootTag.GTag;
import org.genericsystem.reactor.extended.ExtendedRootTag.GTagAnnotation;
import org.genericsystem.reactor.extended.ExtendedRootTag.GTagAnnotationContent;
import org.genericsystem.reactor.gscomponents.TagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(StyleClasses.class)
@Process(StyleClassProcessor.class)
@GenericProcess(StyleClassGenericProcessor.class)
public @interface StyleClass {
	Class<? extends TagImpl>[] path() default {};

	String[] value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface StyleClasses {
		StyleClass[] value();
	}

	public static class StyleClassProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.getRootTag().processStyleClass(tag, ((StyleClass) annotation).value());
		}
	}

	public static class StyleClassGenericProcessor implements IGenericAnnotationProcessor {

		@Override
		public void setAnnotation(GTag gTag, Annotation annotation) {
			gTag.setArrayValueAnnotation(StyleClass.class, null, ((StyleClass) annotation).value(), ((StyleClass) annotation).path(), ((StyleClass) annotation).pos());
		}

		@Override
		public void onRemove(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			for (String styleClass : annotationContent.getContentJSonArray().stream().toArray(String[]::new))
				tag.removeStyleClass(context, styleClass);
		}

		@Override
		public void onAdd(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.getRootTag().processStyleClass(tag, context, annotationContent.getStringArrayContent());
		}
	}
}
