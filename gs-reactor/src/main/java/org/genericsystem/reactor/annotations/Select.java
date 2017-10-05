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
import org.genericsystem.reactor.annotations.Select.SelectGenericProcessor;
import org.genericsystem.reactor.annotations.Select.SelectProcessor;
import org.genericsystem.reactor.annotations.Select.Selects;
import org.genericsystem.reactor.context.GenericSelector;
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
@Repeatable(Selects.class)
@Process(SelectProcessor.class)
@GenericProcess(SelectGenericProcessor.class)
public @interface Select {
	Class<? extends TagImpl>[] path() default {};

	Class<? extends GenericSelector> value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Selects {
		Select[] value();
	}

	public static class SelectProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.getRootTag().processSelect(tag, ((Select) annotation).value());
		}
	}

	public static class SelectGenericProcessor implements MetaBindingAnnotationProcessor {

		@Override
		public void setAnnotation(GTag gTag, Annotation annotation) {
			gTag.setAnnotation(Select.class, null, ((Select) annotation).value().getName(), ((Select) annotation).path(), ((Select) annotation).pos());
		}

		@Override
		public void onAdd(Tag tag, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.getRootTag().processSelect(tag, annotationContent.getClassContent());
		}
	}
}