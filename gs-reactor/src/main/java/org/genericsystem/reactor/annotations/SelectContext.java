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
import org.genericsystem.reactor.annotations.SelectContext.SelectContextGenericProcessor;
import org.genericsystem.reactor.annotations.SelectContext.SelectContextProcessor;
import org.genericsystem.reactor.annotations.SelectContext.SelectContexts;
import org.genericsystem.reactor.context.ObservableContextSelector;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTag;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotation;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotationContent;
import org.genericsystem.reactor.gscomponents.TagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(SelectContexts.class)
@Process(SelectContextProcessor.class)
@GenericProcess(SelectContextGenericProcessor.class)
public @interface SelectContext {
	Class<? extends TagImpl>[] path() default {};

	Class<? extends ObservableContextSelector> value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface SelectContexts {
		SelectContext[] value();
	}

	public static class SelectContextProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.getRootTag().processSelectContext(tag, ((SelectContext) annotation).value());
		}
	}

	public static class SelectContextGenericProcessor implements MetaBindingAnnotationProcessor {

		@Override
		public void setAnnotation(GTag gTag, Annotation annotation) {
			gTag.setAnnotation(SelectContext.class, null, ((SelectContext) annotation).value().getName(), ((SelectContext) annotation).path(), ((SelectContext) annotation).pos());
		}

		@Override
		public void onAdd(Tag tag, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.getRootTag().processSelectContext(tag, annotationContent.getClassContent());
		}
	}
}