package org.genericsystem.reactor;

import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Deque;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindSelection;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.Children.ChildrenProcessor;
import org.genericsystem.reactor.annotations.CustomAnnotations;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Process;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AnnotationsManager {

	private final Set<AnnotationProcessor> processors = new LinkedHashSet<>();

	public static final Logger log = LoggerFactory.getLogger(AnnotationsManager.class);

	public AnnotationsManager(Class<? extends RootTag> clazz) {
		registerCustomAnnotations(clazz);
		initManager();
	}

	private void registerCustomAnnotations(Class<? extends RootTag> clazz) {
		Annotation annotations = clazz.getAnnotation(CustomAnnotations.class);
		if (annotations != null)
			for (Class<? extends Annotation> annotation : ((CustomAnnotations) annotations).value())
				registerAnnotation(annotation);
	}

	public void initManager() {
		registerAnnotation(DirectSelect.class);
		registerAnnotation(Select.class);
		registerAnnotation(SelectContext.class);
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
		registerAnnotation(Attribute.class);
		registerAnnotation(Switch.class);
	}

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

	public void processChildrenAnnotations(Tag tag) {
		try {
			processAnnotation(new AnnotationProcessor(Children.class, ChildrenProcessor.class.newInstance(), false), tag);
		} catch (IllegalAccessException | InstantiationException e) {
			throw new IllegalStateException(e);
		}
	}

	public void processAnnotations(Tag tag) {
		for (AnnotationProcessor processor : processors)
			processAnnotation(processor, tag);
	}

	static <T extends Tag> void processAnnotation(AnnotationProcessor processor, Tag tag) {
		Deque<Class<?>> classesToResult = new ArrayDeque<>();
		Tag current = tag;
		if (!processor.isRepeatable()) {
			Annotation applyingAnnotation = null;
			while (current != null) {
				List<Annotation> annotationsFound = selectAnnotations(current.getClass(), processor.getAnnotationClass(), classesToResult, tag);
				if (!DirectSelect.class.equals(processor.getAnnotationClass())) {
					Class<?> superClass = current.getClass().getSuperclass();
					while (annotationsFound.isEmpty() && Tag.class.isAssignableFrom(superClass)) {
						annotationsFound = selectAnnotations(superClass, processor.getAnnotationClass(), classesToResult, tag);
						superClass = superClass.getSuperclass();
					}
				}
				if (!annotationsFound.isEmpty())
					applyingAnnotation = annotationsFound.get(0);
				classesToResult.push(current.getClass());
				current = current.getParent();
			}
			if (applyingAnnotation != null)
				processor.getProcess().accept(applyingAnnotation, tag);
		} else {
			List<Annotation> applyingAnnotations = new ArrayList<>();
			while (current != null) {
				Class<?> superClass = current.getClass();
				List<Annotation> annotationsFound = new ArrayList<>();
				while (Tag.class.isAssignableFrom(superClass)) {
					annotationsFound.addAll(selectAnnotations(superClass, processor.getAnnotationClass(), classesToResult, tag));
					superClass = superClass.getSuperclass();
				}
				Collections.reverse(annotationsFound);
				applyingAnnotations.addAll(annotationsFound);
				classesToResult.push(current.getClass());
				current = current.getParent();
			}
			for (Annotation applyingAnnotation : applyingAnnotations)
				processor.getProcess().accept(applyingAnnotation, tag);
		}
	}

	public static boolean posMatches(int[] posAnnotation, Class<?>[] pathAnnotation, Tag testedTag) {
		if (posAnnotation.length == 0)
			return true;
		Tag tag = testedTag;
		for (int i = pathAnnotation.length - 1; i >= 0; i--) {
			if (posAnnotation[i] != -1 && position(tag, pathAnnotation[i]) != posAnnotation[i])
				return false;
			tag = tag.getParent();
		}
		return true;
	}

	// Assumes that tag is of a class extending tagClass.
	public static int position(Tag tag, Class<?> tagClass) {
		int result = 0;
		for (Tag sibling : tag.getParent().getObservableChildren()) {
			if (sibling.equals(tag))
				break;
			if (tagClass.isAssignableFrom(sibling.getClass()))
				result++;
		}
		return result;
	}

	static List<Annotation> selectAnnotations(Class<?> annotatedClass, Class<? extends Annotation> annotationClass, Deque<Class<?>> classesToResult, Tag tag) {
		List<Annotation> annotationsFound = new ArrayList<>();
		Annotation[] annotations = annotatedClass.getAnnotationsByType(annotationClass);
		for (Annotation annotation : annotations)
			try {
				Class<?>[] path = (Class<?>[]) annotation.annotationType().getDeclaredMethod("path").invoke(annotation);
				int[] pos = (int[]) annotation.annotationType().getDeclaredMethod("pos").invoke(annotation);
				if (pos.length != 0 && pos.length != path.length)
					throw new IllegalStateException("The annotation " + annotationClass.getSimpleName() + " contains a path and an array of class positions of different lengths. path: "
							+ Arrays.asList(path).stream().map(c -> c.getSimpleName()).collect(Collectors.toList()) + ", positions: " + IntStream.of(pos).boxed().collect(Collectors.toList()) + " found on class " + annotatedClass.getSimpleName());
				if (isAssignableFrom(Arrays.asList(path), new ArrayList<>(classesToResult)) && posMatches(pos, path, tag)) {
					if (!annotationsFound.isEmpty() && !(Style.class.equals(annotationClass) || Attribute.class.equals(annotationClass)))
						throw new IllegalStateException("Multiple annotations applicable to same tag defined at same level. Annotation: " + annotationClass.getSimpleName() + ", path to tag: "
								+ Arrays.asList(path).stream().map(c -> c.getSimpleName()).collect(Collectors.toList()));
					annotationsFound.add(annotation);
				}
			} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
				throw new IllegalStateException(e);
			}
		return annotationsFound;
	}

	public static boolean isAssignableFrom(List<Class<?>> list1, List<Class<?>> list2) {
		if (list1.size() != list2.size())
			return false;
		for (int i = 0; i < list1.size(); i++)
			if (!list1.get(i).isAssignableFrom(list2.get(i)))
				return false;
		return true;
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
