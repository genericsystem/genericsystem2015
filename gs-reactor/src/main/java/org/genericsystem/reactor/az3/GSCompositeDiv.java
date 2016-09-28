package org.genericsystem.reactor.az3;

import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.function.Consumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.ForEach.ChildForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.ReactorDependencies.ChildReactorDependencies;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.Select.ChildSelect;
import org.genericsystem.reactor.annotations.Styles.AlignItems;
import org.genericsystem.reactor.annotations.Styles.BackgroundColor;
import org.genericsystem.reactor.annotations.Styles.ChildAlignItems;
import org.genericsystem.reactor.annotations.Styles.ChildBackgroundColor;
import org.genericsystem.reactor.annotations.Styles.ChildColor;
import org.genericsystem.reactor.annotations.Styles.ChildFlex;
import org.genericsystem.reactor.annotations.Styles.ChildFlexDirection;
import org.genericsystem.reactor.annotations.Styles.ChildFlexWrap;
import org.genericsystem.reactor.annotations.Styles.ChildGenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Styles.ChildHeight;
import org.genericsystem.reactor.annotations.Styles.ChildJustifyContent;
import org.genericsystem.reactor.annotations.Styles.ChildKeepFlexDirection;
import org.genericsystem.reactor.annotations.Styles.ChildMarginBottom;
import org.genericsystem.reactor.annotations.Styles.ChildMarginRight;
import org.genericsystem.reactor.annotations.Styles.ChildOverflow;
import org.genericsystem.reactor.annotations.Styles.ChildReverseFlexDirection;
import org.genericsystem.reactor.annotations.Styles.ChildStyle;
import org.genericsystem.reactor.annotations.Styles.ChildWidth;
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
		initComposite();
		processAnnotations(getClass(), this);
		init();
	}

	private void initComposite() {
		nodes.put(getClass(), this);
		processAnnotation(ReactorDependencies.class, getClass(), annotation -> {
			// System.out.println("Declaring classes : " + Arrays.toString(getClass().getDeclaredClasses()));
			// System.out.println("ReactorDependencies : " + Arrays.toString(deps.value()));
			for (Class<? extends GSTagImpl> clazz : ((ReactorDependencies) annotation).value())
				find(clazz);
		});
		if (getParent() != null) {
			processDecorateAnnotation(ChildReactorDependencies.class, getParent().getClass(), this, annotation -> {
				for (Class<? extends GSTagImpl> childClass : ((ChildReactorDependencies) annotation).value())
					find(childClass);
			});
		}
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
			if (GSCompositeDiv.class.isAssignableFrom(tagClass))
				((GSCompositeDiv) result).initComposite();
			processAnnotations(tagClass, result);
			result.init();
		}
		return result;
	}

	private static <T extends Tag> void processAnnotations(Class<T> tagClass, Tag result) {
		if (result.getParent() != null) {
			Class<? extends Tag> parentClass = result.getParent().getClass();

			processDecorateAnnotation(ChildSelect.class, parentClass, result, annotation -> {
				try {
					result.select(((ChildSelect) annotation).value().newInstance().get());
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			});
			processDecorateAnnotation(ChildForEach.class, parentClass, result, annotation -> {
				try {
					result.forEach(((ChildForEach) annotation).value().newInstance().get());
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			});
			processDecorateAnnotation(ChildStyle.class, parentClass, result, annotation -> result.addStyle(((ChildStyle) annotation).name(), ((ChildStyle) annotation).value()));

			processDecorateAnnotation(ChildFlexDirection.class, parentClass, result, annotation -> {
				if (GSDiv.class.isAssignableFrom(result.getClass()))
					((GSDiv) result).setDirection(((ChildFlexDirection) annotation).value());
				else
					log.warn("Warning: FlexDirection is applicable only to GSDiv extensions.");
			});
			processDecorateAnnotation(ChildKeepFlexDirection.class, parentClass, result, annotation -> {
				if (GSDiv.class.isAssignableFrom(result.getClass()))
					((GSDiv) result).keepDirection();
				else
					log.warn("Warning: KeepFlexDirection is applicable only to GSDiv extensions.");
			});
			processDecorateAnnotation(ChildReverseFlexDirection.class, parentClass, result, annotation -> {
				if (GSDiv.class.isAssignableFrom(result.getClass()))
					((GSDiv) result).reverseDirection();
				else
					log.warn("Warning: ReverseFlexDirection is applicable only to GSDiv extensions.");
			});

			processDecorateAnnotation(ChildGenericValueBackgroundColor.class, parentClass, result, annotation -> result.addPrefixBinding(modelContext -> result.addStyle(modelContext, "background-color",
					"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? ((GenericStringDefaults) result).getGenericStringProperty(modelContext).getValue() : "#e5ed00")));

			processDecorateStyleAnnotation(ChildFlex.class, parentClass, result, "flex");
			processDecorateStyleAnnotation(ChildFlexWrap.class, parentClass, result, "flex-wrap");
			processDecorateStyleAnnotation(ChildBackgroundColor.class, parentClass, result, "background-color");
			processDecorateStyleAnnotation(ChildAlignItems.class, parentClass, result, "align-items");
			processDecorateStyleAnnotation(ChildJustifyContent.class, parentClass, result, "justify-content");
			processDecorateStyleAnnotation(ChildOverflow.class, parentClass, result, "overflow");
			processDecorateStyleAnnotation(ChildColor.class, parentClass, result, "color");
			processDecorateStyleAnnotation(ChildMarginRight.class, parentClass, result, "margin-right");
			processDecorateStyleAnnotation(ChildMarginBottom.class, parentClass, result, "margin-bottom");
			processDecorateStyleAnnotation(ChildHeight.class, parentClass, result, "height");
			processDecorateStyleAnnotation(ChildWidth.class, parentClass, result, "width");
		}

		processAnnotation(ForEach.class, tagClass, annotation -> {
			try {
				result.forEach(((ForEach) annotation).value().newInstance().get());
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		});

		processAnnotation(Select.class, tagClass, annotation -> {
			try {
				result.select(((Select) annotation).value().newInstance().get());
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		});

		processAnnotation(FlexDirectionStyle.class, tagClass, annotation -> {
			if (GSDiv.class.isAssignableFrom(result.getClass()))
				((GSDiv) result).setDirection(((FlexDirectionStyle) annotation).value());
			else
				log.warn("Warning: FlexDirectionStyle is applicable only to GSDiv extensions.");
		});
		processAnnotation(KeepFlexDirection.class, tagClass, annotation -> {
			if (GSDiv.class.isAssignableFrom(result.getClass()))
				((GSDiv) result).keepDirection();
			else
				log.warn("Warning: KeepFlexDirection is applicable only to GSDiv extensions.");
		});
		processAnnotation(ReverseFlexDirection.class, tagClass, annotation -> {
			if (GSDiv.class.isAssignableFrom(result.getClass()))
				((GSDiv) result).reverseDirection();
			else
				log.warn("Warning: ReverseFlexDirection is applicable only to GSDiv extensions.");
		});
		processAnnotation(DirectSelect.class, tagClass, annotation -> result.select(((DirectSelect) annotation).value()));
		processAnnotation(GenericValueBackgroundColor.class, tagClass, annotation -> result.addPrefixBinding(modelContext -> result.addStyle(modelContext, "background-color",
				"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? ((GenericStringDefaults) result).getGenericStringProperty(modelContext).getValue() : "#e5ed00")));

		processAnnotation(Style.class, tagClass, annotation -> result.addStyle(((Style) annotation).propertyName(), ((Style) annotation).propertyValue()));

		processStyleAnnotation(BackgroundColor.class, tagClass, result, "background-color");
		processStyleAnnotation(FlexWrap.class, tagClass, result, "flex-wrap");
		processStyleAnnotation(Flex.class, tagClass, result, "flex");
		processStyleAnnotation(AlignItems.class, tagClass, result, "align-items");
		processStyleAnnotation(JustifyContent.class, tagClass, result, "justify-content");
		processStyleAnnotation(Overflow.class, tagClass, result, "overflow");
		processStyleAnnotation(Color.class, tagClass, result, "color");
		processStyleAnnotation(MarginRight.class, tagClass, result, "margin-right");
		processStyleAnnotation(MarginBottom.class, tagClass, result, "margin-bottom");
		processStyleAnnotation(Height.class, tagClass, result, "height");
		processStyleAnnotation(Width.class, tagClass, result, "width");
	}

	private static <T extends Tag> void processAnnotation(Class<? extends Annotation> annotationClass, Class<T> tagClass, Consumer<Annotation> consumer) {
		Annotation[] annotations = tagClass.getAnnotationsByType(annotationClass);
		for (Annotation annotation : annotations)
			consumer.accept(annotation);
	}

	private static <T extends Tag> void processStyleAnnotation(Class<? extends Annotation> annotationClass, Class<T> tagClass, Tag result, String propertyName) {
		processAnnotation(annotationClass, tagClass, annotation -> {
			try {
				result.addStyle(propertyName, (String) annotation.annotationType().getDeclaredMethod("value").invoke(annotation));
			} catch (Exception e) {
				throw new IllegalStateException(e);
			}
		});
	}

	private static <T extends Tag> void processDecorateAnnotation(Class<? extends Annotation> annotationClass, Class<T> tagClass, Tag result, Consumer<Annotation> consumer) {
		Annotation[] annotations = tagClass.getAnnotationsByType(annotationClass);
		for (Annotation annotation : annotations)
			try {
				if (((Class<?>) annotation.annotationType().getDeclaredMethod("decorate").invoke(annotation)).isAssignableFrom(result.getClass()))
					consumer.accept(annotation);
			} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
				throw new IllegalStateException(e);
			}
	}

	private static <T extends Tag> void processDecorateStyleAnnotation(Class<? extends Annotation> annotationClass, Class<T> tagClass, Tag result, String propertyName) {
		processDecorateAnnotation(annotationClass, tagClass, result, annotation -> {
			try {
				result.addStyle(propertyName, (String) annotation.annotationType().getDeclaredMethod("value").invoke(annotation));
			} catch (Exception e) {
				throw new IllegalStateException(e);
			}
		});
	}
}
