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
import org.genericsystem.reactor.annotations.Stepper.StepperGenericProcessor;
import org.genericsystem.reactor.annotations.Stepper.StepperProcessor;
import org.genericsystem.reactor.gscomponents.Controller;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTag;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotation;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotationContent;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.TagType.TagAnnotationAttribute;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.TagType.TagAnnotationContentAttribute;
import org.genericsystem.reactor.gscomponents.TagImpl;

import io.vertx.core.json.JsonObject;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Process(StepperProcessor.class)
@GenericProcess(StepperGenericProcessor.class)
public @interface Stepper {

	Class<? extends TagImpl> first();

	Class<? extends TagImpl>[] path() default {};

	int[] pos() default {};

	public static class StepperProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			Controller.initialize(tag, ((Stepper) annotation).first());
		}
	}

	public static class StepperGenericProcessor implements IGenericAnnotationProcessor {

		@Override
		public void setAnnotation(GTag gTag, Annotation annotation) {
			Stepper stepper = (Stepper) annotation;
			GTagAnnotation gTagAnnotation = (GTagAnnotation) gTag.setHolder(gTag.getRoot().find(TagAnnotationAttribute.class), new TagAnnotation(Stepper.class, stepper.path(), stepper.pos()));
			gTagAnnotation.setHolder(gTag.getRoot().find(TagAnnotationContentAttribute.class), new JsonObject().put("first", stepper.first().getName()).encodePrettily());
		}

		@Override
		public void onRemove(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			if (tag.getMetaBinding() == null)
				context.removeTag(tag);
			else
				context.getParent().removeTag(tag);
		}

		@Override
		public void onAdd(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			if (tag.getMetaBinding() == null)
				context.addTag(tag);
			else
				context.getParent().addTag(tag);
		}

		@Override
		public void onRemove(Tag tag, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.addPrefixBinding(context -> context.getProperties(tag).remove(Controller.CONTROLLER));
		}

		@Override
		public void onAdd(Tag tag, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			JsonObject content = gTagAnnotation.getContent().getJsonValue();
			Class<? extends TagImpl> first = null;
			try {
				first = (Class<? extends TagImpl>) Class.forName(content.getString("first"));
			} catch (ClassNotFoundException e) {
				throw new IllegalStateException("Class " + content.getString("first") + " not found");
			}
			Controller.initialize(tag, first);
		}
	}
}
