package org.genericsystem.reactor.az3;

import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.Styles.AlignItems;
import org.genericsystem.reactor.annotations.Styles.BackgroundColor;
import org.genericsystem.reactor.annotations.Styles.Color;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.FlexWrap;
import org.genericsystem.reactor.annotations.Styles.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Styles.Height;
import org.genericsystem.reactor.annotations.Styles.JustifyContent;
import org.genericsystem.reactor.annotations.Styles.KeepFlexDirection;
import org.genericsystem.reactor.annotations.Styles.MarginBottom;
import org.genericsystem.reactor.annotations.Styles.MarginRight;
import org.genericsystem.reactor.annotations.Styles.Overflow;
import org.genericsystem.reactor.annotations.Styles.ReverseFlexDirection;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.annotations.Styles.Width;
import org.genericsystem.reactor.az.GSDiv;
import org.genericsystem.reactor.az.GSTagImpl;
import org.genericsystem.reactor.model.StringExtractor;
import org.genericsystem.reactor.modelproperties.GenericStringDefaults;

import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class GSCompositeDiv extends GSDiv implements Tag {

	public static final Logger log = LoggerFactory.getLogger(Tag.class);

	private final HashMap<Class<? extends Tag>, Tag> nodes = new LinkedHashMap<Class<? extends Tag>, Tag>() {
		private static final long serialVersionUID = -6835018021862236920L;

		@Override
		public Tag get(Object key) {
			Class<? extends Tag> searchClass = (Class<? extends Tag>) key;
			Tag tag = super.get(searchClass);
			if (tag == null)
				for (Class<? extends Tag> clazz : keySet()) {
					if (searchClass.isAssignableFrom(clazz)) {
						if (tag == null) {
							tag = super.get(clazz);
							if (!searchClass.equals(GSCompositeDiv.this.getClass()))
								System.out.println("Search : " + searchClass.getSimpleName() + " find polymorphic class : " + GSCompositeDiv.this.getClass().getSimpleName());
							else
								break;
						} else
							System.out.println("Warning : Found several results for class : " + searchClass.getSimpleName() + " on : " + GSCompositeDiv.this.getClass().getSimpleName() + " exact paths for them : " + searchClass + " " + getClass());
					}
				}
			return tag;
		};

	};

	public GSCompositeDiv() {
	}

	public GSCompositeDiv(Tag parent) {
		super(parent);
		processAnnotations(this);
		initComposite();
		init();
	}

	private void initComposite() {
		nodes.put(getClass(), this);
		processAnnotation(ReactorDependencies.class, this, annotation -> {
			// System.out.println("Declaring classes : " + Arrays.toString(getClass().getDeclaredClasses()));
			// System.out.println("ReactorDependencies : " + Arrays.toString(deps.value()));
			for (Class<? extends GSTagImpl> clazz : ((ReactorDependencies) annotation).value())
				find(clazz);
		});
		for (Tag tag : nodes.values())
			tag.postfix();
	}

	@Override
	public <T extends Tag> T find(Class<T> tagClass) {
		T result = (T) nodes.get(tagClass);
		if (result == null) {
			try {
				result = tagClass.newInstance();
			} catch (IllegalAccessException | InstantiationException e) {
				throw new IllegalStateException(e);
			}
			((GSTagImpl) result).setParent(this);
			processAnnotations(result);
			if (GSCompositeDiv.class.isAssignableFrom(tagClass))
				((GSCompositeDiv) result).initComposite();
			result.init();
			nodes.put(tagClass, result);
		}
		return result;
	}

	private static <T extends Tag> void processAnnotations(Tag result) {
		processAnnotation(DirectSelect.class, result, annotation -> result.select(((DirectSelect) annotation).value()));
		processAnnotation(Select.class, result, annotation -> {
			try {
				result.select(((Select) annotation).value().newInstance().get());
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		});
		processAnnotation(ForEach.class, result, annotation -> {
			try {
				result.forEach(((ForEach) annotation).value().newInstance().get());
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		});
		processAnnotation(Style.class, result, annotation -> result.addStyle(((Style) annotation).name(), ((Style) annotation).value()));

		processAnnotation(FlexDirectionStyle.class, result, annotation -> {
			if (GSDiv.class.isAssignableFrom(result.getClass()))
				((GSDiv) result).setDirection(((FlexDirectionStyle) annotation).value());
			else
				log.warn("Warning: FlexDirection is applicable only to GSDiv extensions.");
		});
		processAnnotation(KeepFlexDirection.class, result, annotation -> {
			if (GSDiv.class.isAssignableFrom(result.getClass()))
				((GSDiv) result).keepDirection();
			else
				log.warn("Warning: KeepFlexDirection is applicable only to GSDiv extensions.");
		});
		processAnnotation(ReverseFlexDirection.class, result, annotation -> {
			if (GSDiv.class.isAssignableFrom(result.getClass()))
				((GSDiv) result).reverseDirection();
			else
				log.warn("Warning: ReverseFlexDirection is applicable only to GSDiv extensions.");
		});

		processStyleAnnotation(Flex.class, result, "flex");
		processStyleAnnotation(FlexWrap.class, result, "flex-wrap");
		processStyleAnnotation(BackgroundColor.class, result, "background-color");
		processStyleAnnotation(AlignItems.class, result, "align-items");
		processStyleAnnotation(JustifyContent.class, result, "justify-content");
		processStyleAnnotation(Overflow.class, result, "overflow");
		processStyleAnnotation(Color.class, result, "color");
		processStyleAnnotation(MarginRight.class, result, "margin-right");
		processStyleAnnotation(MarginBottom.class, result, "margin-bottom");
		processStyleAnnotation(Height.class, result, "height");
		processStyleAnnotation(Width.class, result, "width");
		processAnnotation(GenericValueBackgroundColor.class, result, annotation -> result.addPrefixBinding(modelContext -> result.addStyle(modelContext, "background-color",
				"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? ((GenericStringDefaults) result).getGenericStringProperty(modelContext).getValue() : "#e5ed00")));
	}

	private static boolean isAssignableFrom(List<Class<?>> list1, List<Class<?>> list2) {
		if (list1.size() != list2.size())
			return false;
		for (int i = 0; i < list1.size(); i++)
			if (!list1.get(i).isAssignableFrom(list2.get(i)))
				return false;
		return true;
	}

	private static <T extends Tag> void processAnnotation(Class<? extends Annotation> annotationClass, Tag result, Consumer<Annotation> consumer) {
		List<Class<?>> classesToResult = new ArrayList<>();
		Tag current = result;
		Annotation applyingAnnotation = null;
		while (current != null) {
			Annotation[] annotations = current.getClass().getAnnotationsByType(annotationClass);
			Annotation annotationFound = selectAnnotation(annotations, annotationClass, classesToResult);

			if (!DirectSelect.class.equals(annotationClass)) {
				Class<?> superClass = current.getClass().getSuperclass();
				while (annotationFound == null && superClass != null) {
					annotations = superClass.getAnnotationsByType(annotationClass);
					annotationFound = selectAnnotation(annotations, annotationClass, classesToResult);
					superClass = superClass.getSuperclass();
				}
			}
			if (annotationFound != null)
				applyingAnnotation = annotationFound;
			classesToResult.add(0, current.getClass());
			current = current.getParent();
		}
		if (applyingAnnotation != null)
			consumer.accept(applyingAnnotation);
	}

	private static Annotation selectAnnotation(Annotation[] annotations, Class<? extends Annotation> annotationClass, List<Class<?>> classesToResult) {
		Annotation annotationFound = null;
		for (Annotation annotation : annotations)
			try {
				Class<?>[] decorate = (Class<?>[]) annotation.annotationType().getDeclaredMethod("decorate").invoke(annotation);
				if (isAssignableFrom(Arrays.asList(decorate), classesToResult)) {
					if (annotationFound != null)
						throw new IllegalStateException("Multiple annotations applicable to same tag defined at same level. Annotation: " + annotationClass.getSimpleName() + ", path to tag: "
								+ Arrays.asList(decorate).stream().map(c -> c.getSimpleName()).collect(Collectors.toList()));
					annotationFound = annotation;
				}
			} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
				throw new IllegalStateException(e);
			}
		return annotationFound;
	}

	private static <T extends Tag> void processStyleAnnotation(Class<? extends Annotation> annotationClass, Tag result, String propertyName) {
		processAnnotation(annotationClass, result, annotation -> {
			try {
				result.addStyle(propertyName, (String) annotation.annotationType().getDeclaredMethod("value").invoke(annotation));
			} catch (Exception e) {
				throw new IllegalStateException(e);
			}
		});
	}
}
