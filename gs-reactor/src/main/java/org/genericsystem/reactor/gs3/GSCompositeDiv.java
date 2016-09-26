package org.genericsystem.reactor.gs3;

import java.util.HashMap;
import java.util.LinkedHashMap;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.ForEach.ChildForEach;
import org.genericsystem.reactor.annotations.ForEach.ParentForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.ReactorDependencies.ChildReactorDependencies;
import org.genericsystem.reactor.annotations.ReactorDependencies.ChildReactorDependenciesMult;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.Styles;
import org.genericsystem.reactor.annotations.Styles.AlignItems;
import org.genericsystem.reactor.annotations.Styles.BackgroundColor;
import org.genericsystem.reactor.annotations.Styles.ChildFlexDirection;
import org.genericsystem.reactor.annotations.Styles.Color;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.FlexWrap;
import org.genericsystem.reactor.annotations.Styles.GenericBackgroundColor;
import org.genericsystem.reactor.annotations.Styles.Height;
import org.genericsystem.reactor.annotations.Styles.JustifyContent;
import org.genericsystem.reactor.annotations.Styles.KeepFlexDirection;
import org.genericsystem.reactor.annotations.Styles.MarginBottom;
import org.genericsystem.reactor.annotations.Styles.MarginRight;
import org.genericsystem.reactor.annotations.Styles.Overflow;
import org.genericsystem.reactor.annotations.Styles.ParentFlexDirection;
import org.genericsystem.reactor.annotations.Styles.ReverseFlexDirection;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.annotations.Styles.Width;
import org.genericsystem.reactor.gs.GSDiv;
import org.genericsystem.reactor.gs.GSTagImpl;
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

		ReactorDependencies dependencies = getClass().getAnnotation(ReactorDependencies.class);
		if (dependencies != null) {
			// System.out.println("Declaring classes : " + Arrays.toString(getClass().getDeclaredClasses()));
			// System.out.println("ReactorDependencies : " + Arrays.toString(deps.value()));
			for (Class<? extends GSTagImpl> clazz : dependencies.value())
				find(clazz);
		}
		if (getParent() != null) {
			ChildReactorDependenciesMult childDependenciesMult = getParent().getClass().getAnnotation(ChildReactorDependenciesMult.class);
			if (childDependenciesMult != null) {
				for (ChildReactorDependencies cd : childDependenciesMult.value()) {
					if (cd.decorate().isAssignableFrom(getClass()))
						find(cd.value());
				}
			} else {
				ChildReactorDependencies childDependencies = getParent().getClass().getAnnotation(ChildReactorDependencies.class);
				if (childDependencies != null) {
					if (childDependencies.decorate().isAssignableFrom(getClass()))
						find(childDependencies.value());
				} else
					log.warn("Warning : unable to find ChildReactorDependencies on : " + getParent().getClass().getSimpleName() + " for : " + getClass().getSimpleName());
			}
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
			nodes.put(tagClass, result);
		}
		return result;
	}

	private static <T extends Tag> void processAnnotations(Class<T> tagClass, Tag result) {
		ParentForEach parentForEach = tagClass.getAnnotation(ParentForEach.class);
		if (parentForEach == null) {
			ForEach forEach = tagClass.getAnnotation(ForEach.class);
			if (forEach != null) {
				try {
					result.forEach(forEach.value().newInstance().get());
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			}
		} else {

			ChildForEach childForEach = result.getParent().getClass().getAnnotation(ChildForEach.class);
			if (childForEach != null) {
				try {
					result.forEach(childForEach.value()[parentForEach.pos()].newInstance().get());
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			} else
				log.warn("Warning : unable to find childForEach on : " + result.getParent().getClass().getSimpleName() + " for : " + tagClass.getSimpleName());
		}

		Select select = tagClass.getAnnotation(Select.class);
		if (select != null) {
			try {
				result.select(select.value().newInstance().get());
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		}
		ParentFlexDirection parentFlexDirection = tagClass.getAnnotation(ParentFlexDirection.class);
		if (parentFlexDirection == null) {
			FlexDirectionStyle flexDirection = tagClass.getAnnotation(FlexDirectionStyle.class);
			if (flexDirection != null) {
				if (GSDiv.class.isAssignableFrom(result.getClass()))
					((GSDiv) result).setDirection(flexDirection.value());
				else
					log.warn("Warning: FlexDirectionStyle is applicable only to GSDiv extensions.");
			}
		} else {
			ChildFlexDirection childFlexDirection = result.getParent().getClass().getAnnotation(ChildFlexDirection.class);
			if (childFlexDirection != null)
				result.addStyle("flex-direction", childFlexDirection.value()[parentFlexDirection.pos()]);
		}

		if (tagClass.getAnnotation(KeepFlexDirection.class) != null)
			if (GSDiv.class.isAssignableFrom(result.getClass()))
				((GSDiv) result).keepDirection();
			else
				log.warn("Warning: KeepFlexDirection is applicable only to GSDiv extensions.");
		if (tagClass.getAnnotation(ReverseFlexDirection.class) != null)
			if (GSDiv.class.isAssignableFrom(result.getClass()))
				((GSDiv) result).reverseDirection();
			else
				log.warn("Warning: ReverseFlexDirection is applicable only to GSDiv extensions.");
		DirectSelect directSelect = tagClass.getAnnotation(DirectSelect.class);
		if (directSelect != null)
			result.select(directSelect.value());
		BackgroundColor backgroundColor = tagClass.getAnnotation(BackgroundColor.class);
		if (backgroundColor != null)
			result.addStyle("background-color", backgroundColor.value());

		if (tagClass.getAnnotation(GenericBackgroundColor.class) != null)
			result.addPrefixBinding(modelContext -> result.addStyle(modelContext, "background-color",
					"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? ((GenericStringDefaults) result).getGenericStringProperty(modelContext).getValue() : "#e5ed00"));
		FlexWrap flexWrap = tagClass.getAnnotation(FlexWrap.class);
		if (flexWrap != null)
			result.addStyle("flex-wrap", flexWrap.value());
		Flex flex = tagClass.getAnnotation(Flex.class);
		if (flex != null)
			result.addStyle("flex", flex.value());
		AlignItems alignItems = tagClass.getAnnotation(AlignItems.class);
		if (alignItems != null)
			result.addStyle("align-items", alignItems.value());
		JustifyContent justifyContent = tagClass.getAnnotation(JustifyContent.class);
		if (justifyContent != null)
			result.addStyle("justify-content", justifyContent.value());
		Overflow overlflow = tagClass.getAnnotation(Overflow.class);
		if (overlflow != null)
			result.addStyle("overflow", overlflow.value());
		Color color = tagClass.getAnnotation(Color.class);
		if (color != null)
			result.addStyle("color", color.value());
		MarginRight marginRight = tagClass.getAnnotation(MarginRight.class);
		if (marginRight != null)
			result.addStyle("margin-right", marginRight.value());
		MarginBottom marginBottom = tagClass.getAnnotation(MarginBottom.class);
		if (marginBottom != null)
			result.addStyle("margin-bottom", marginBottom.value());
		Height height = tagClass.getAnnotation(Height.class);
		if (height != null)
			result.addStyle("height", height.value());
		Width width = tagClass.getAnnotation(Width.class);
		if (width != null)
			result.addStyle("width", width.value());

		Style style = tagClass.getAnnotation(Style.class);
		if (style != null)
			result.addStyle(style.propertyName(), style.propertyValue());
		Styles styles = tagClass.getAnnotation(Styles.class);
		if (styles != null)
			for (Style style_ : styles.value())
				result.addStyle(style_.propertyName(), style_.propertyValue());
	}
}