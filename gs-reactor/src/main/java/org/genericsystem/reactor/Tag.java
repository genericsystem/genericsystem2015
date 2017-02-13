package org.genericsystem.reactor;

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BindingsTools;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.contextproperties.ActionDefaults;
import org.genericsystem.reactor.contextproperties.AttributesDefaults;
import org.genericsystem.reactor.contextproperties.DisplayDefaults;
import org.genericsystem.reactor.contextproperties.GSBuilderDefaults;
import org.genericsystem.reactor.contextproperties.GenericStringDefaults;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.contextproperties.StepperDefaults;
import org.genericsystem.reactor.contextproperties.StyleClassesDefaults;
import org.genericsystem.reactor.contextproperties.StylesDefaults;
import org.genericsystem.reactor.contextproperties.TextPropertyDefaults;
import org.genericsystem.reactor.contextproperties.UserRoleDefaults;
import org.genericsystem.reactor.gscomponents.TagImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.beans.binding.ListBinding;
import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.MapChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.util.StringConverter;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 */
public interface Tag extends TagNode, ActionDefaults, SelectionDefaults, StepperDefaults, GSBuilderDefaults, TextPropertyDefaults, StylesDefaults, AttributesDefaults, StyleClassesDefaults, GenericStringDefaults, DisplayDefaults, UserRoleDefaults {

	public static final Logger log = LoggerFactory.getLogger(Tag.class);

	public String getTag();

	public void setTag(String tagName);

	public Class<? extends HtmlDomNode> getDomNodeClass();

	public void setDomNodeClass(Class<? extends HtmlDomNode> domNodeClass);

	public List<Consumer<Context>> getPreFixedBindings();

	public List<Consumer<Context>> getPostFixedBindings();

	public <BETWEEN> MetaBinding<BETWEEN> getMetaBinding();

	public <BETWEEN> Property<MetaBinding<BETWEEN>> getMetaBindingProperty();

	public <BETWEEN> void setMetaBinding(MetaBinding<BETWEEN> metaBinding);

	@Override
	default void addPrefixBinding(Consumer<Context> consumer) {
		getPreFixedBindings().add(consumer);
	}

	@Override
	default void addPostfixBinding(Consumer<Context> consumer) {
		getPostFixedBindings().add(consumer);
	}

	default void bindOptionalStyleClass(String styleClass, String propertyName) {
		addPrefixBinding(modelContext -> {
			ObservableValue<Boolean> optional = getContextObservableValue(propertyName, modelContext);
			Set<String> styleClasses = getDomNodeStyleClasses(modelContext);
			Consumer<Boolean> consumer = bool -> {
				if (Boolean.TRUE.equals(bool))
					styleClasses.add(styleClass);
				else
					styleClasses.remove(styleClass);
			};
			consumer.accept(optional.getValue());
			optional.addListener((o, ov, nv) -> consumer.accept(nv));
		});
	}

	default void bindOptionalStyleClass(String styleClass, String modelPropertyName, Function<Context, ObservableValue<Boolean>> applyOnModel) {
		addContextAttribute(modelPropertyName, applyOnModel);
		bindOptionalStyleClass(styleClass, modelPropertyName);
	}

	default void forEach2(Function<Context, ObservableList<Generic>> applyOnModel) {
		setMetaBinding(MetaBinding.forEachMetaBinding(applyOnModel));
	}

	default void forEach(ObservableListExtractor observableListExtractor) {
		forEach2(model -> observableListExtractor.apply(model.getGenerics()));
	}

	default void forEach(Tag parent) {
		forEach(gs -> parent.getObservableListExtractor().apply(gs));
	}

	default void select(Function<Generic[], Generic> genericSupplier) {
		forEach((ObservableListExtractor) gs -> {
			Generic generic = genericSupplier.apply(gs);
			return generic != null ? FXCollections.singletonObservableList(generic) : FXCollections.emptyObservableList();
		});
	}

	default void select_(Function<Context, ObservableList<Context>> applyOnModel) {
		setMetaBinding(MetaBinding.selectMetaBinding(applyOnModel));
	}

	default void select__(Function<Context, ObservableValue<Context>> applyOnModelContext) {
		// fix probable issues with transmitSuccessiveInvalidations
		select_(context -> BindingsTools.transmitSuccessiveInvalidations(new ListBinding<Context>() {
			ObservableValue<Context> ov = applyOnModelContext.apply(context);
			{
				bind(ov);
			}

			@Override
			protected ObservableList<Context> computeValue() {
				Context context_ = ov.getValue();
				return context_ != null ? FXCollections.singletonObservableList(context_) : FXCollections.emptyObservableList();
			}
		}));
	}

	default void select(Class<?> genericClass) {
		forEach((ObservableListExtractor) gs -> FXCollections.singletonObservableList(gs[0].getRoot().find(genericClass)));
	}

	default ObservableListExtractor getObservableListExtractor() {
		return ObservableListExtractor.SUBINSTANCES;
	}

	@Override
	default <T> T getContextAttribute(String attributeName, Context context) {
		class AttributeComputer {
			T getAttribute(String attributeName, Context[] contexts) {
				Tag tag = Tag.this;
				while (tag != null && contexts[0] != null) {
					if (contexts[0].containsAttribute(tag, attributeName))
						return contexts[0].<T> getAttribute(tag, attributeName);
					if (tag.getMetaBinding() != null && contexts[0].getHtmlDomNode(tag.getParent()) == null)
						contexts[0] = contexts[0].getParent();
					tag = tag.getParent();
				}
				return null;
			}
		}
		return new AttributeComputer().getAttribute(attributeName, new Context[] { context });
	}

	@Override
	default <T> Property<T> getContextProperty(String propertyName, Context context) {
		return getContextAttribute(propertyName, context);
	}

	@Override
	default <T> ObservableValue<T> getContextObservableValue(String propertyName, Context context) {
		return getContextAttribute(propertyName, context);
	}

	@Override
	default <T> T getInheritedContextAttribute(String attributeName, Context[] context, Tag[] tag) {
		while (tag[0] != null && context[0] != null) {
			if (tag[0].getMetaBinding() != null && context[0].getHtmlDomNode(tag[0].getParent()) == null)
				context[0] = context[0].getParent();
			tag[0] = tag[0].getParent();
			if (context[0] != null && context[0].containsAttribute(tag[0], attributeName))
				return context[0].<T> getAttribute(tag[0], attributeName);
		}
		return null;
	}

	@Override
	default <T extends Serializable> void addContextPropertyChangeListener(String propertyName, BiConsumer<Context, T> listener) {
		addPrefixBinding(context -> {
			ObservableValue<T> observable = getContextObservableValue(propertyName, context);
			observable.addListener((o, old, nva) -> listener.accept(context, nva));
		});
	}

	/**
	 * Creates a new context attribute with the given name and value. Throws an exception if an attribute with this name already exists.
	 * 
	 * @throws IllegalStateException
	 *             If an attribute with this name already exists.
	 */
	@Override
	default <T> void addContextAttribute(String attributeName, Function<Context, T> applyOnModel) {
		addPrefixBinding(context -> context.addContextAttribute(this, attributeName, applyOnModel.apply(context)));
	}

	/**
	 * Creates a new context attribute for the given context with the given name and value. Throws an exception if an attribute with this name already exists.
	 * 
	 * @throws IllegalStateException
	 *             If an attribute with this name already exists.
	 */
	@Override
	default <T> void addContextAttribute(String attributeName, Context context, T value) {
		context.addContextAttribute(this, attributeName, value);
	}

	/**
	 * Sets the attribute with the given name to the given value. If the attribute does not exist yet, it is created, otherwise it is replaced.
	 */
	@Override
	default <T> void setContextAttribute(String attributeName, Context context, T value) {
		context.setContextAttribute(this, attributeName, value);
	}

	/**
	 * Sets the attribute with the given name to the given value. If the attribute does not exist yet, it is created, otherwise it is replaced.
	 */
	@Override
	default <T> void setContextAttribute(String attributeName, Function<Context, T> getValue) {
		addPrefixBinding(context -> setContextAttribute(attributeName, context, getValue.apply(context)));
	}

	/**
	 * Sets the value of a context attribute that is a property. If no attribute with the given name exists, one is created, otherwise the existing property’s value is replaced.
	 * 
	 * @param attributeName
	 *            The attribute’s name.
	 * @param context
	 *            The context to use.
	 * @param value
	 *            The value of the property contained in the attribute.
	 */
	@Override
	default <T> void setContextPropertyValue(String attributeName, Context context, T value) {
		context.setContextPropertyValue(this, attributeName, value);
	}

	/**
	 * Sets the value of a context attribute that is a property. If no attribute with the given name exists, one is created, otherwise the existing property’s value is replaced.
	 * 
	 * @param attributeName
	 *            The attribute’s name.
	 * @param getValue
	 *            The method to apply to the context to retrieve the value to use.
	 */
	@Override
	default <T> void setContextPropertyValue(String attributeName, Function<Context, T> getValue) {
		addPrefixBinding(context -> setContextPropertyValue(attributeName, context, getValue.apply(context)));
	}

	default <T> void setInheritedContextPropertyValue(String attributeName, Context context, T value) {
		Property<T> contextProperty = getContextProperty(attributeName, context);
		if (contextProperty == null)
			setContextPropertyValue(attributeName, context, value);
		else
			contextProperty.setValue(value);
	}

	/**
	 * Creates a new context attribute with the given name containing an empty property. Throws an exception if an attribute with this name already exists.
	 */
	@Override
	default void createNewContextProperty(String propertyName) {
		addPrefixBinding(context -> context.createNewContextProperty(this, propertyName));
	}

	/**
	 * Creates a new context attribute with the given name containing an empty property. Throws an exception if an attribute with this name already exists.
	 */
	default void createNewContextProperty(String propertyName, Context context) {
		context.createNewContextProperty(this, propertyName);
	}

	/**
	 * Creates a new context attribute with the given name containing a property with the given value. Throws an exception if an attribute with this name already exists.
	 */
	@Override
	default <T> void createNewInitializedProperty(String propertyName, Context context, T initialValue) {
		context.createNewContextProperty(this, propertyName);
		getContextProperty(propertyName, context).setValue(initialValue);
	}

	/**
	 * Creates a new context attribute with the given name containing a property with the value obtained by applying getInitialValue to the context. Throws an exception if an attribute with this name already exists.
	 */
	@Override
	default <T> void createNewInitializedProperty(String propertyName, Function<Context, T> getInitialValue) {
		addPrefixBinding(context -> createNewInitializedProperty(propertyName, context, getInitialValue.apply(context)));
	}

	default void bindMapElement(String name, String propertyName, Function<Context, Map<String, String>> getMap) {
		addPrefixBinding(model -> {
			Map<String, String> map = getMap.apply(model);
			ChangeListener<String> listener = (o, old, newValue) -> map.put(name, newValue);
			ObservableValue<String> observable = getContextObservableValue(propertyName, model);
			observable.addListener(listener);
			map.put(name, observable.getValue());
		});
	}

	default void bindBiDirectionalMapElement(String propertyName, String name, Function<Context, ObservableMap<String, String>> getMap) {
		bindBiDirectionalMapElement(propertyName, name, getMap, ReactorStatics.STRING_CONVERTERS.get(String.class));
	}

	default <T extends Serializable> void bindBiDirectionalMapElement(String propertyName, String name, Function<Context, ObservableMap<String, String>> getMap, StringConverter<T> stringConverter) {
		bindBiDirectionalMapElement(propertyName, name, getMap, model -> stringConverter);
	}

	default <T extends Serializable> void bindBiDirectionalMapElement(String propertyName, String name, Function<Context, ObservableMap<String, String>> getMap, Function<Context, StringConverter<T>> getStringConverter) {
		addPrefixBinding(modelContext -> {
			ObservableMap<String, String> map = getMap.apply(modelContext);
			StringConverter<T> stringConverter = getStringConverter.apply(modelContext);
			ChangeListener<T> listener = (o, old, newValue) -> map.put(name, stringConverter.toString(newValue));
			Property<T> observable = getContextProperty(propertyName, modelContext);
			observable.addListener(listener);
			map.addListener((MapChangeListener<String, String>) c -> {
				if (!name.equals(c.getKey()))
					return;
				try {
					observable.setValue(c.wasAdded() ? stringConverter.fromString(c.getValueAdded()) : null);
				} catch (Exception ignore) {
					log.warn("Conversion exception : " + ignore.getMessage());
				}
			});
			map.put(name, stringConverter.toString(observable.getValue()));
		});
	}

	default void addStyle(String propertyName, String value) {
		addPrefixBinding(context -> addStyle(context, propertyName, value));
	}

	@Override
	default void addStyle(Context context, String propertyName, String value) {
		getDomNodeStyles(context).put(propertyName, value);
	}

	default void bindStyle(String style, String modelPropertyName) {
		bindMapElement(style, modelPropertyName, model -> getDomNodeStyles(model));
	}

	default void bindStyle(String style, String propertyName, Function<Context, ObservableValue<String>> applyOnModel) {
		addContextAttribute(propertyName, applyOnModel);
		bindMapElement(style, propertyName, model -> getDomNodeStyles(model));
	}

	default void addStyleClasses(String... styleClasses) {
		addPrefixBinding(model -> getDomNodeStyleClasses(model).addAll(Arrays.asList(styleClasses)));
	}

	default void addStyleClass(String styleClass) {
		addPrefixBinding(model -> getDomNodeStyleClasses(model).add(styleClass));
	}

	default void addStyleClass(Context context, String styleClass) {
		getDomNodeStyleClasses(context).add(styleClass);
	}

	default void addAttribute(String attributeName, String value) {
		addPrefixBinding(model -> getDomNodeAttributes(model).put(attributeName, value));
	}

	default void addAttribute(Context context, String attributeName, String value) {
		getDomNodeAttributes(context).put(attributeName, value);
	}

	default void removeStyleClass(Context context, String styleClass) {
		getDomNodeStyleClasses(context).remove(styleClass);
	}

	default void removeStyleClass(String styleClass) {
		addPrefixBinding(context -> getDomNodeStyleClasses(context).remove(styleClass));
	}

	default void bindAttribute(String attributeName, String propertyName) {
		bindMapElement(attributeName, propertyName, model -> getDomNodeAttributes(model));
	}

	default void bindAttribute(String attributeName, String propertyName, Function<Context, ObservableValue<String>> applyOnModel) {
		addContextAttribute(propertyName, applyOnModel);
		bindMapElement(attributeName, propertyName, model -> getDomNodeAttributes(model));
	}

	default void bindBiDirectionalAttribute(String propertyName, String attributeName) {
		bindBiDirectionalMapElement(propertyName, attributeName, model -> getDomNodeAttributes(model));
	}

	default <T extends Serializable> void bindBiDirectionalAttribute(String propertyName, String attributeName, StringConverter<T> stringConverter) {
		bindBiDirectionalMapElement(propertyName, attributeName, model -> getDomNodeAttributes(model), stringConverter);
	}

	default <T extends Serializable> void bindBiDirectionalAttribute(String propertyName, String attributeName, Function<Context, StringConverter<T>> getStringConverter) {
		bindBiDirectionalMapElement(propertyName, attributeName, model -> getDomNodeAttributes(model), getStringConverter);
	}

	default void bindOptionalBiDirectionalAttribute(String propertyName, String attributeName, String attributeValue) {
		bindOptionalBiDirectionalAttribute(propertyName, attributeName, attributeValue, null);
	}

	default void bindOptionalBiDirectionalAttribute(String propertyName, String attributeName, String attributeValue, String attributeValueFalse) {
		bindBiDirectionalMapElement(propertyName, attributeName, model -> getDomNodeAttributes(model), new StringConverter<Boolean>() {

			@Override
			public String toString(Boolean bool) {
				return Boolean.TRUE.equals(bool) ? attributeValue : attributeValueFalse;
			}

			@Override
			public Boolean fromString(String string) {
				return attributeValue.equals(string);
			}
		});
	}

	default HtmlDomNode createNode(HtmlDomNode parent, Context context) {
		try {
			return getDomNodeClass().getConstructor(HtmlDomNode.class, Context.class, Tag.class).newInstance(parent, context, this);
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			throw new IllegalStateException(e);
		}
	};

	default <T extends TagImpl> TagImpl createChild(Class<T> clazz) {
		T result = null;
		try {
			result = clazz.newInstance();
		} catch (IllegalAccessException | InstantiationException e) {
			throw new IllegalStateException(e);
		}
		result.initTag(this);
		return result;
	}

	default void initTag(Tag parent) {
		setParent(parent);
		setTagNode(getRootTag().buildTagNode(this));
		getRootTag().processAnnotations(this);
		init();
		parent.getObservableChildren().add(this);
	}

	@Override
	public ObservableList<Tag> getObservableChildren();

	public TagNode getTagNode();

	public void setTagNode(TagNode tagNode);

	default void init() {
	}

	public <COMPONENT extends Tag> COMPONENT getParent();

	public void setParent(Tag parent);

	default RootTag getRootTag() {
		return getParent().getRootTag();
	}

	public ObservableList<TagSwitcher> getObservableSwitchers();

	public void addSwitcher(TagSwitcher switcher);

	default String defaultToString() {
		return getTag() + " " + getClass().getName();
	}

	default <T extends Tag> T find(Class<T> tagClass) {
		return find(tagClass, 0);
	}

	default <T extends Tag> T find(Class<T> tagClass, int pos) {
		int posFound = 0;
		for (Tag child : getObservableChildren()) {
			if (tagClass.isAssignableFrom(child.getClass())) {
				if (posFound == pos)
					return (T) child;
				posFound++;
			}
		}
		throw new IllegalStateException("No tag corresponding to class " + tagClass.getSimpleName() + " found, position " + pos + ".");
	}
}
