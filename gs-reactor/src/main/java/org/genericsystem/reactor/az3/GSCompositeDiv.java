package org.genericsystem.reactor.az3;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.function.Consumer;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.MetaBinding;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.ForEach.ChildForEach;
import org.genericsystem.reactor.annotations.ForEach.ParentForEach;
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
import org.genericsystem.reactor.annotations.Styles.ChildGenericBackgroundColor;
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
import org.genericsystem.reactor.annotations.Styles.GenericBackgroundColor;
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
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javassist.util.proxy.MethodFilter;
import javassist.util.proxy.MethodHandler;
import javassist.util.proxy.ProxyFactory;
import javassist.util.proxy.ProxyObject;

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
			ChildReactorDependencies[] childDependenciesMult = getParent().getClass().getAnnotationsByType(ChildReactorDependencies.class);
			for (ChildReactorDependencies cd : childDependenciesMult) {
				if (cd.decorate().isAssignableFrom(getClass()))
					for (Class<? extends GSTagImpl> childClass : cd.value())
						find(childClass);
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

		if (result.getParent() != null) {
			Class<?> parentClass = result.getParent().getClass();

			ChildSelect[] childSelects = parentClass.getAnnotationsByType(ChildSelect.class);
			for (ChildSelect childSelect : childSelects)
				if (childSelect.decorate().isAssignableFrom(result.getClass()))
					try {
						result.select(childSelect.value().newInstance().get());
					} catch (InstantiationException | IllegalAccessException e) {
						throw new IllegalStateException(e);
					}

			ChildStyle[] childStyles = parentClass.getAnnotationsByType(ChildStyle.class);
			for (ChildStyle childStyle : childStyles)
				if (childStyle.decorate().isAssignableFrom(result.getClass()))
					result.addStyle(childStyle.name(), childStyle.value());

			ChildFlexDirection[] childFlexDirections = parentClass.getAnnotationsByType(ChildFlexDirection.class);
			for (ChildFlexDirection childFlexDirection : childFlexDirections)
				if (childFlexDirection.decorate().isAssignableFrom(result.getClass()))
					if (GSDiv.class.isAssignableFrom(result.getClass()))
						((GSDiv) result).setDirection(childFlexDirection.value());
					else
						log.warn("Warning: FlexDirection is applicable only to GSDiv extensions.");

			ChildKeepFlexDirection childKeepFlexDirection = result.getParent().getClass().getAnnotation(ChildKeepFlexDirection.class);
			if (childKeepFlexDirection != null)
				for (Class<? extends GSTagImpl> clazz : childKeepFlexDirection.value())
					if (clazz.isAssignableFrom(result.getClass()))
						if (GSDiv.class.isAssignableFrom(result.getClass()))
							((GSDiv) result).keepDirection();
						else
							log.warn("Warning: KeepFlexDirection is applicable only to GSDiv extensions.");

			ChildReverseFlexDirection childReverseFlexDirection = parentClass.getAnnotation(ChildReverseFlexDirection.class);
			if (childReverseFlexDirection != null)
				for (Class<? extends GSTagImpl> clazz : childReverseFlexDirection.value())
					if (clazz.isAssignableFrom(result.getClass()))
						if (GSDiv.class.isAssignableFrom(result.getClass()))
							((GSDiv) result).reverseDirection();
						else
							log.warn("Warning: ReverseFlexDirection is applicable only to GSDiv extensions.");

			ChildFlex[] childFlexs = parentClass.getAnnotationsByType(ChildFlex.class);
			for (ChildFlex childFlex : childFlexs)
				if (childFlex.decorate().isAssignableFrom(result.getClass()))
					result.addStyle("flex", childFlex.value());
			ChildFlexWrap[] childFlexWraps = parentClass.getAnnotationsByType(ChildFlexWrap.class);
			for (ChildFlexWrap childFlexWrap : childFlexWraps)
				if (childFlexWrap.decorate().isAssignableFrom(result.getClass()))
					result.addStyle("flex-wrap", childFlexWrap.value());
			ChildBackgroundColor[] childBackgroundColors = parentClass.getAnnotationsByType(ChildBackgroundColor.class);
			for (ChildBackgroundColor childBackgroundColor : childBackgroundColors)
				if (childBackgroundColor.decorate().isAssignableFrom(result.getClass()))
					result.addStyle("background-color", childBackgroundColor.value());
			ChildGenericBackgroundColor[] childGenericBackgroundColors = parentClass.getAnnotationsByType(ChildGenericBackgroundColor.class);
			for (ChildGenericBackgroundColor childGenericBackgroundColor : childGenericBackgroundColors)
				if (childGenericBackgroundColor.decorate().isAssignableFrom(result.getClass()))
					result.addPrefixBinding(modelContext -> result.addStyle(modelContext, "background-color",
							"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? ((GenericStringDefaults) result).getGenericStringProperty(modelContext).getValue() : "#e5ed00"));
			ChildAlignItems[] childAlignItemss = parentClass.getAnnotationsByType(ChildAlignItems.class);
			for (ChildAlignItems childAlignItems : childAlignItemss)
				if (childAlignItems.decorate().isAssignableFrom(result.getClass()))
					result.addStyle("align-items", childAlignItems.value());
			ChildJustifyContent[] childJustifyContents = parentClass.getAnnotationsByType(ChildJustifyContent.class);
			for (ChildJustifyContent childJustifyContent : childJustifyContents)
				if (childJustifyContent.decorate().isAssignableFrom(result.getClass()))
					result.addStyle("justify-content", childJustifyContent.value());
			ChildOverflow[] childOverflows = parentClass.getAnnotationsByType(ChildOverflow.class);
			for (ChildOverflow childOverflow : childOverflows)
				if (childOverflow.decorate().isAssignableFrom(result.getClass()))
					result.addStyle("overflow", childOverflow.value());
			ChildColor[] childColors = parentClass.getAnnotationsByType(ChildColor.class);
			for (ChildColor childColor : childColors)
				if (childColor.decorate().isAssignableFrom(result.getClass()))
					result.addStyle("color", childColor.value());
			ChildMarginRight[] childMarginRights = parentClass.getAnnotationsByType(ChildMarginRight.class);
			for (ChildMarginRight childMarginRight : childMarginRights)
				if (childMarginRight.decorate().isAssignableFrom(result.getClass()))
					result.addStyle("margin-right", childMarginRight.value());
			ChildMarginBottom[] childMarginBottoms = parentClass.getAnnotationsByType(ChildMarginBottom.class);
			for (ChildMarginBottom childMarginBottom : childMarginBottoms)
				if (childMarginBottom.decorate().isAssignableFrom(result.getClass()))
					result.addStyle("margin-bottom", childMarginBottom.value());
			ChildHeight[] childHeights = parentClass.getAnnotationsByType(ChildHeight.class);
			for (ChildHeight childHeight : childHeights)
				if (childHeight.decorate().isAssignableFrom(result.getClass()))
					result.addStyle("height", childHeight.value());
			ChildWidth[] childWidths = parentClass.getAnnotationsByType(ChildWidth.class);
			for (ChildWidth childWidth : childWidths)
				if (childWidth.decorate().isAssignableFrom(result.getClass()))
					result.addStyle("width", childWidth.value());
		}

		FlexDirectionStyle flexDirection = tagClass.getAnnotation(FlexDirectionStyle.class);
		if (flexDirection != null) {
			if (GSDiv.class.isAssignableFrom(result.getClass()))
				((GSDiv) result).setDirection(flexDirection.value());
			else
				log.warn("Warning: FlexDirectionStyle is applicable only to GSDiv extensions.");
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

		Style[] styles = tagClass.getAnnotationsByType(Style.class);
		for (Style style : styles)
			result.addStyle(style.propertyName(), style.propertyValue());
	}

	private final static ProxyFactory PROXY_FACTORY = new ProxyFactory();
	private final static MethodFilter METHOD_FILTER = method -> method.getName().equals("toString");

	private Tag newInstance(Tag parent, Class<? extends Tag> clazz) {
		assert clazz.isInterface();
		PROXY_FACTORY.setSuperclass(Object.class);
		PROXY_FACTORY.setInterfaces(new Class[] { clazz });
		Tag result;
		try {
			result = (Tag) PROXY_FACTORY.createClass(METHOD_FILTER).newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			throw new IllegalStateException(e);
		}
		((ProxyObject) result).setHandler(new TagHandler(parent, result));
		return result;
	}

	private static class TagHandler implements MethodHandler {

		private MetaBinding<?> metaBinding;
		private final List<Consumer<Context>> preFixedBindings = new ArrayList<>();
		private final List<Consumer<Context>> postFixedBindings = new ArrayList<>();
		private Tag parent;
		private final ObservableList<Tag> children = FXCollections.observableArrayList();

		protected TagHandler(Tag parent, Tag tag) {
			this.parent = parent;
			if (parent != null)
				parent.getObservableChildren().add(tag);
		}

		@Override
		public Object invoke(Object self, Method m, Method proceed, Object[] args) throws Throwable {
			return ((Tag) self).defaultToString();
		}

		public List<Consumer<Context>> getPreFixedBindings() {
			return preFixedBindings;
		}

		public List<Consumer<Context>> getPostFixedBindings() {
			return postFixedBindings;
		}

		@SuppressWarnings("unchecked")
		public <BETWEEN> MetaBinding<BETWEEN> getMetaBinding() {
			return (MetaBinding<BETWEEN>) metaBinding;
		}

		public <BETWEEN> void setMetaBinding(MetaBinding<BETWEEN> metaBinding) {
			if (this.metaBinding != null)
				throw new IllegalStateException("MetaBinding already defined");
			this.metaBinding = metaBinding;
		}

		public ObservableList<Tag> getObservableChildren() {
			return children;
		}

		@SuppressWarnings("unchecked")
		public <COMPONENT extends Tag> COMPONENT getParent() {
			return (COMPONENT) parent;
		}
	}

}
