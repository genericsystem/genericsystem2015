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
		processAnnotations(this);
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
		if (getParent() != null) {
			processDecorateAnnotation(ChildReactorDependencies.class, this, annotation -> {
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
			processAnnotations(result);
			result.init();
		}
		return result;
	}

	private static <T extends Tag> void processAnnotations(Tag result) {
		if (result.getParent() != null) {
			processDecorateAnnotation(ChildSelect.class, result, annotation -> {
				try {
					result.select(((ChildSelect) annotation).value().newInstance().get());
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			});
			processDecorateAnnotation(ChildForEach.class, result, annotation -> {
				try {
					result.forEach(((ChildForEach) annotation).value().newInstance().get());
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			});
			processDecorateAnnotation(ChildStyle.class, result, annotation -> result.addStyle(((ChildStyle) annotation).name(), ((ChildStyle) annotation).value()));

			processDecorateAnnotation(ChildFlexDirection.class, result, annotation -> {
				if (GSDiv.class.isAssignableFrom(result.getClass()))
					((GSDiv) result).setDirection(((ChildFlexDirection) annotation).value());
				else
					log.warn("Warning: FlexDirection is applicable only to GSDiv extensions.");
			});
			processDecorateAnnotation(ChildKeepFlexDirection.class, result, annotation -> {
				if (GSDiv.class.isAssignableFrom(result.getClass()))
					((GSDiv) result).keepDirection();
				else
					log.warn("Warning: KeepFlexDirection is applicable only to GSDiv extensions.");
			});
			processDecorateAnnotation(ChildReverseFlexDirection.class, result, annotation -> {
				if (GSDiv.class.isAssignableFrom(result.getClass()))
					((GSDiv) result).reverseDirection();
				else
					log.warn("Warning: ReverseFlexDirection is applicable only to GSDiv extensions.");
			});

			processDecorateAnnotation(ChildGenericValueBackgroundColor.class, result, annotation -> result.addPrefixBinding(modelContext -> result.addStyle(modelContext, "background-color",
					"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? ((GenericStringDefaults) result).getGenericStringProperty(modelContext).getValue() : "#e5ed00")));

			processDecorateStyleAnnotation(ChildFlex.class, result, "flex");
			processDecorateStyleAnnotation(ChildFlexWrap.class, result, "flex-wrap");
			processDecorateStyleAnnotation(ChildBackgroundColor.class, result, "background-color");
			processDecorateStyleAnnotation(ChildAlignItems.class, result, "align-items");
			processDecorateStyleAnnotation(ChildJustifyContent.class, result, "justify-content");
			processDecorateStyleAnnotation(ChildOverflow.class, result, "overflow");
			processDecorateStyleAnnotation(ChildColor.class, result, "color");
			processDecorateStyleAnnotation(ChildMarginRight.class, result, "margin-right");
			processDecorateStyleAnnotation(ChildMarginBottom.class, result, "margin-bottom");
			processDecorateStyleAnnotation(ChildHeight.class, result, "height");
			processDecorateStyleAnnotation(ChildWidth.class, result, "width");
		}

		processAnnotation(ForEach.class, result, annotation -> {
			try {
				result.forEach(((ForEach) annotation).value().newInstance().get());
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		});

		processAnnotation(Select.class, result, annotation -> {
			try {
				result.select(((Select) annotation).value().newInstance().get());
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		});

		processAnnotation(FlexDirectionStyle.class, result, annotation -> {
			if (GSDiv.class.isAssignableFrom(result.getClass()))
				((GSDiv) result).setDirection(((FlexDirectionStyle) annotation).value());
			else
				log.warn("Warning: FlexDirectionStyle is applicable only to GSDiv extensions.");
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
		processAnnotation(DirectSelect.class, result, annotation -> result.select(((DirectSelect) annotation).value()));
		processAnnotation(GenericValueBackgroundColor.class, result, annotation -> result.addPrefixBinding(modelContext -> result.addStyle(modelContext, "background-color",
				"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? ((GenericStringDefaults) result).getGenericStringProperty(modelContext).getValue() : "#e5ed00")));

		processAnnotation(Style.class, result, annotation -> result.addStyle(((Style) annotation).propertyName(), ((Style) annotation).propertyValue()));

		processStyleAnnotation(BackgroundColor.class, result, "background-color");
		processStyleAnnotation(FlexWrap.class, result, "flex-wrap");
		processStyleAnnotation(Flex.class, result, "flex");
		processStyleAnnotation(AlignItems.class, result, "align-items");
		processStyleAnnotation(JustifyContent.class, result, "justify-content");
		processStyleAnnotation(Overflow.class, result, "overflow");
		processStyleAnnotation(Color.class, result, "color");
		processStyleAnnotation(MarginRight.class, result, "margin-right");
		processStyleAnnotation(MarginBottom.class, result, "margin-bottom");
		processStyleAnnotation(Height.class, result, "height");
		processStyleAnnotation(Width.class, result, "width");
	}

	private static <T extends Tag> void processAnnotation(Class<? extends Annotation> annotationClass, Tag result, Consumer<Annotation> consumer) {
		Annotation[] annotations = result.getClass().getAnnotationsByType(annotationClass);
		for (Annotation annotation : annotations)
			consumer.accept(annotation);
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

	private static <T extends Tag> void processDecorateAnnotation(Class<? extends Annotation> annotationClass, Tag result, Consumer<Annotation> consumer) {
		Annotation[] annotations = result.getParent().getClass().getAnnotationsByType(annotationClass);
		for (Annotation annotation : annotations)
			try {
				if (((Class<?>) annotation.annotationType().getDeclaredMethod("decorate").invoke(annotation)).isAssignableFrom(result.getClass()))
					consumer.accept(annotation);
			} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
				throw new IllegalStateException(e);
			}
	}

	private static <T extends Tag> void processDecorateStyleAnnotation(Class<? extends Annotation> annotationClass, Tag result, String propertyName) {
		processDecorateAnnotation(annotationClass, result, annotation -> {
			try {
				result.addStyle(propertyName, (String) annotation.annotationType().getDeclaredMethod("value").invoke(annotation));
			} catch (Exception e) {
				throw new IllegalStateException(e);
			}
		});
	}
}
