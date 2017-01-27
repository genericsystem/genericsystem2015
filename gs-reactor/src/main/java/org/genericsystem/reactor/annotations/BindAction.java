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
import org.genericsystem.reactor.annotations.BindAction.BindActionGenericProcessor;
import org.genericsystem.reactor.annotations.BindAction.BindActionProcessor;
import org.genericsystem.reactor.annotations.BindAction.BindActions;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.contextproperties.ActionDefaults;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTag;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotation;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotationContent;
import org.genericsystem.reactor.gscomponents.TagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(BindActions.class)
@Process(BindActionProcessor.class)
@GenericProcess(BindActionGenericProcessor.class)
public @interface BindAction {
	Class<? extends TagImpl>[] path() default {};

	Class<? extends ContextAction>[] value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface BindActions {
		BindAction[] value();
	}

	public static class BindActionProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.getRootTag().processBindAction(tag, ((BindAction) annotation).value());
		}
	}

	public static class BindActionGenericProcessor implements IGenericAnnotationProcessor {

		@Override
		public void setAnnotation(GTag gTag, Annotation annotation) {
			gTag.setArrayValueAnnotation(BindAction.class, null, ((BindAction) annotation).value(), ((BindAction) annotation).path(), ((BindAction) annotation).pos());
		}

		@Override
		public void onRemove(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			context.getProperties(tag).remove(ActionDefaults.ACTION);
		}

		@Override
		public void onAdd(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.getRootTag().processBindAction(tag, context, (Class<? extends ContextAction>[]) annotationContent.getClassArrayContent());
		}
	}
}