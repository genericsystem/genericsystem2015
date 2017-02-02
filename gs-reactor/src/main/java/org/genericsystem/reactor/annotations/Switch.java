package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.DirectSelect.AnnotationProcessorNoActionWithContext;
import org.genericsystem.reactor.annotations.Switch.Modes;
import org.genericsystem.reactor.annotations.Switch.SwichProcessor;
import org.genericsystem.reactor.annotations.Switch.SwitchGenericProcessor;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTag;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotation;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotationContent;
import org.genericsystem.reactor.gscomponents.TagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(Modes.class)
@Process(SwichProcessor.class)
@GenericProcess(SwitchGenericProcessor.class)
public @interface Switch {
	Class<? extends TagImpl>[] path() default {};

	int[] pos() default {};

	Class<? extends TagSwitcher>[] value();

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Modes {
		Switch[] value();
	}

	public static class SwichProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.getRootTag().processSwitch(tag, ((Switch) annotation).value());
		}
	}

	public static class SwitchGenericProcessor implements AnnotationProcessorNoActionWithContext {

		@Override
		public void setAnnotation(GTag gTag, Annotation annotation) {
			gTag.setArrayValueAnnotation(Switch.class, null, ((Switch) annotation).value(), ((Switch) annotation).path(), ((Switch) annotation).pos());
		}

		@Override
		public void onRemove(Tag tag, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			for (Class<? extends TagSwitcher> switcherClass : (Class<? extends TagSwitcher>[]) annotationContent.getClassArrayContent())
				tag.getObservableSwitchers().removeAll(tag.getObservableSwitchers().filtered(switcher -> switcherClass.equals(switcher.getClass())));
		}

		@Override
		public void onAdd(Tag tag, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.getRootTag().processSwitch(tag, (Class<? extends TagSwitcher>[]) annotationContent.getClassArrayContent());
		}
	}
}
