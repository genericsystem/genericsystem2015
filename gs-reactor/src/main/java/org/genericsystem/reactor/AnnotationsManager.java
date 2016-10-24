package org.genericsystem.reactor;

import java.lang.annotation.Annotation;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindSelection;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Process;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SelectModel;
import org.genericsystem.reactor.annotations.SetStringExtractor;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Stepper;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Style.KeepFlexDirection;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.annotations.StyleClass;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AnnotationsManager {

	private static final AnnotationsManager INSTANCE = new AnnotationsManager();

	private final Set<AnnotationProcessor> processors = new LinkedHashSet<>();

	public static final Logger log = LoggerFactory.getLogger(AnnotationsManager.class);

	public void registerAnnotation(Class<? extends Annotation> annotationClass) {
		Annotation processAnnotation = annotationClass.getAnnotation(Process.class);
		if (processAnnotation != null) {
			try {
				processors.add(new AnnotationProcessor(annotationClass, ((Process) processAnnotation).value().newInstance(), ((Process) processAnnotation).repeatable()));
			} catch (IllegalAccessException | InstantiationException e) {
				throw new IllegalStateException(e);
			}
		} else
			log.warn("Unable to find a processor on annotation : " + annotationClass.getSimpleName());
	}

	private AnnotationsManager() {
		registerAnnotation(Children.class);
		registerAnnotation(DirectSelect.class);
		registerAnnotation(Select.class);
		registerAnnotation(SelectModel.class);
		registerAnnotation(ForEach.class);
		registerAnnotation(Stepper.class);
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
	}

	public static AnnotationsManager getInstance() {
		return INSTANCE;
	}

	public Set<AnnotationProcessor> getProcessors() {
		return processors;
	}

	public class AnnotationProcessor {

		private final Class<? extends Annotation> annotationClass;
		private final BiConsumer<Annotation, Tag> process;
		private final boolean repeatable;

		public AnnotationProcessor(Class<? extends Annotation> annotationClass, BiConsumer<Annotation, Tag> process, boolean repeatable) {
			this.annotationClass = annotationClass;
			this.process = process;
			this.repeatable = repeatable;
		}

		public Class<? extends Annotation> getAnnotationClass() {
			return annotationClass;
		}

		public BiConsumer<Annotation, Tag> getProcess() {
			return process;
		}

		public boolean isRepeatable() {
			return repeatable;
		}
	}

}
