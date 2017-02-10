package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.api.core.TagAnnotation;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.ExtendedAnnotationsManager.IGenericAnnotationProcessor;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Step.StepGenericProcessor;
import org.genericsystem.reactor.annotations.Step.StepProcessor;
import org.genericsystem.reactor.annotations.Step.Steps;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTag;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotation;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotationContent;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.TagType.TagAnnotationAttribute;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.TagType.TagAnnotationContentAttribute;
import org.genericsystem.reactor.gscomponents.TagImpl;

import io.vertx.core.json.JsonObject;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Process(StepProcessor.class)
@GenericProcess(StepGenericProcessor.class)
@Repeatable(Steps.class)
public @interface Step {
	Class<? extends TagImpl>[] path() default {};

	int[] pos() default {};

	Class<? extends TagImpl> next();

	String prevText() default "<<";

	String nextText() default ">>";

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Steps {
		Step[] value();
	}

	public static class StepProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.getRootTag().processStep(tag, ((Step) annotation).next(), ((Step) annotation).prevText(), ((Step) annotation).nextText());
		}
	}

	public static class StepGenericProcessor implements IGenericAnnotationProcessor {

		@Override
		public void setAnnotation(GTag gTag, Annotation annotation) {
			Step step = (Step) annotation;
			GTagAnnotation gTagAnnotation = (GTagAnnotation) gTag.setHolder(gTag.getRoot().find(TagAnnotationAttribute.class), new TagAnnotation(Step.class, step.path(), step.pos()));
			gTagAnnotation.setHolder(gTag.getRoot().find(TagAnnotationContentAttribute.class), new JsonObject().put("next", step.next().getName()).put("prevText", step.prevText()).put("nextText", step.nextText()).encodePrettily());
		}

		@Override
		public void onRemove(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			// Nothing to do.
		}

		@Override
		public void onAdd(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			// Nothing to do.
		}

		@SuppressWarnings("unchecked")
		@Override
		public void onAdd(Tag tag, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			JsonObject content = gTagAnnotation.getContent().getJsonValue();
			Class<? extends TagImpl> nextClass = null;
			try {
				nextClass = (Class<? extends TagImpl>) Class.forName(content.getString("next"));
			} catch (ClassNotFoundException e) {
				throw new IllegalStateException("Class " + content.getString("next") + " not found");
			}
			tag.getRootTag().processStep(tag, nextClass, content.getString("prevText"), content.getString("nextText"));
		}
	}
}
