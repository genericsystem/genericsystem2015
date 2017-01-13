package org.genericsystem.reactor;

import java.lang.annotation.Annotation;
import java.util.LinkedHashMap;
import java.util.Map;

import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindSelection;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.GenericProcess;
import org.genericsystem.reactor.annotations.InheritStyle;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SelectContext;
import org.genericsystem.reactor.annotations.SetStringExtractor;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Stepper;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Style.KeepFlexDirection;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTag;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotation;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotationContent;
import org.genericsystem.reactor.gscomponents.TagImpl;

public class ExtendedAnnotationsManager extends AnnotationsManager {

	private Map<Class<? extends Annotation>, IGenericAnnotationProcessor> processors;

	public Map<Class<? extends Annotation>, IGenericAnnotationProcessor> getProcessors() {
		return processors;
	}

	public ExtendedAnnotationsManager(Class<? extends RootTag> clazz) {
		super(clazz);
	}

	@Override
	public void initManager(Class<? extends RootTag> clazz) {
		processors = new LinkedHashMap<>();
		registerAnnotation(DirectSelect.class);
		registerAnnotation(Select.class);
		registerAnnotation(SelectContext.class);
		registerAnnotation(ForEach.class);
		super.registerAnnotation(Stepper.class);
		registerAnnotation(BindSelection.class);
		registerAnnotation(SetStringExtractor.class);
		registerAnnotation(StyleClass.class);
		registerAnnotation(FlexDirectionStyle.class);
		registerAnnotation(KeepFlexDirection.class);
		registerAnnotation(ReverseFlexDirection.class);
		registerAnnotation(SetText.class);
		registerAnnotation(BindText.class);
		registerAnnotation(BindAction.class);
		registerAnnotation(Style.class);
		registerAnnotation(GenericValueBackgroundColor.class);
		registerAnnotation(InheritStyle.class);
		registerAnnotation(Attribute.class);
		registerAnnotation(Switch.class);
		registerCustomAnnotations(clazz);
	}

	@Override
	public void processChildrenAnnotations(Tag tag) {
		((TagImpl) tag).setTagNode(tag.getRootTag().buildTagNode(tag));
	}

	@Override
	public void registerAnnotation(Class<? extends Annotation> annotationClass) {
		GenericProcess processAnnotation = annotationClass.getAnnotation(GenericProcess.class);
		if (processAnnotation != null) {
			try {
				processors.put(annotationClass, processAnnotation.value().newInstance());
			} catch (IllegalAccessException | InstantiationException e) {
				throw new IllegalStateException(e);
			}
		} else
			log.warn("Unable to find a generic processor on annotation : " + annotationClass.getSimpleName());
	}

	public static interface IGenericAnnotationProcessor {

		void setAnnotation(GTag gTag, Annotation annotation);

		void onRemove(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent);

		void onAdd(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent);

		default void onRemove(Tag tag, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.addPrefixBinding(context -> onRemove(tag, context, gTagAnnotation, annotationContent));
		}

		default void onAdd(Tag tag, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.addPrefixBinding(context -> onAdd(tag, context, gTagAnnotation, annotationContent));
		}
	}
}
