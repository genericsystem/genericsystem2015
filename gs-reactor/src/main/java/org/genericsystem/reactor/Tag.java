package org.genericsystem.reactor;

import org.genericsystem.reactor.modelproperties.AttributesDefaults;
import org.genericsystem.reactor.modelproperties.DisplayDefaults;
import org.genericsystem.reactor.modelproperties.GenericStringDefaults;
import org.genericsystem.reactor.modelproperties.StyleClassesDefaults;
import org.genericsystem.reactor.modelproperties.StylesDefaults;
import org.genericsystem.reactor.modelproperties.TextPropertyDefaults;

import io.vertx.core.http.ServerWebSocket;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.binding.ListBinding;
import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.MapChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.util.StringConverter;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BindingsTools;
import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.gscomponents.GSTagImpl;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 */
public interface Tag extends TextPropertyDefaults, StylesDefaults, AttributesDefaults, StyleClassesDefaults, GenericStringDefaults, DisplayDefaults {

	public static final Logger log = LoggerFactory.getLogger(Tag.class);

	public String getTag();

	public List<Consumer<Context>> getPreFixedBindings();

	public List<Consumer<Context>> getPostFixedBindings();

	public <BETWEEN> MetaBinding<BETWEEN> getMetaBinding();

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
			ObservableValue<Boolean> optional = getObservableValue(propertyName, modelContext);
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
		storeProperty(modelPropertyName, applyOnModel);
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
		select_(model -> BindingsTools.transmitSuccessiveInvalidations(new ListBinding<Context>() {
			ObservableValue<Context> ov = applyOnModelContext.apply(model);
			{
				bind(ov);
			}

			@Override
			protected ObservableList<Context> computeValue() {
				Context model = ov.getValue();
				return model != null ? FXCollections.singletonObservableList(model) : FXCollections.emptyObservableList();
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
	default <T> Property<T> getProperty(String propertyName, Context model) {
		class PropertyComputer {
			Property<T> getProperty(String propertyName, Context[] models) {
				Tag tag = Tag.this;
				while (tag != null && models[0] != null) {
					if (models[0].containsProperty(tag, propertyName))
						return models[0].getProperty(tag, propertyName);
					if (tag.getMetaBinding() != null && models[0].getHtmlDomNode(tag.getParent()) == null)
						models[0] = models[0].getParent();
					tag = tag.getParent();
				}
				return null;
			}
		}
		return new PropertyComputer().getProperty(propertyName, new Context[] { model });
	}

	@Override
	default <T> Property<T> getInheritedProperty(String propertyName, Context[] model, Tag[] tag) {
		while (tag != null && model[0] != null) {
			if (tag[0].getMetaBinding() != null && model[0].getHtmlDomNode(tag[0].getParent()) == null)
				model[0] = model[0].getParent();
			tag[0] = tag[0].getParent();
			if (model[0] != null && model[0].containsProperty(tag[0], propertyName))
				return model[0].<T> getProperty(tag[0], propertyName);
		}
		return null;
	}

	@Override
	default <T> ObservableValue<T> getObservableValue(String propertyName, Context model) {

		class ObservableValueComputer {
			ObservableValue<T> getObservableValue(String propertyName, Context[] model) {
				Tag tag = Tag.this;
				while (tag != null && model[0] != null) {
					if (model[0].containsProperty(tag, propertyName))
						return model[0].<T> getObservableValue(tag, propertyName);
					if (tag.getMetaBinding() != null && model[0].getHtmlDomNode(tag.getParent()) == null)
						model[0] = model[0].getParent();
					tag = tag.getParent();
				}
				return null;
			}
		}
		return new ObservableValueComputer().getObservableValue(propertyName, new Context[] { model });
	}

	default void bindMapElement(String name, String propertyName, Function<Context, Map<String, String>> getMap) {
		addPrefixBinding(model -> {
			Map<String, String> map = getMap.apply(model);
			ChangeListener<String> listener = (o, old, newValue) -> map.put(name, newValue);
			ObservableValue<String> observable = getObservableValue(propertyName, model);
			observable.addListener(listener);
			map.put(name, observable.getValue());
		});
	}

	default void bindBiDirectionalMapElement(String propertyName, String name, Function<Context, ObservableMap<String, String>> getMap) {
		bindBiDirectionalMapElement(propertyName, name, getMap, ApiStatics.STRING_CONVERTERS.get(String.class));
	}

	default <T extends Serializable> void bindBiDirectionalMapElement(String propertyName, String name, Function<Context, ObservableMap<String, String>> getMap, StringConverter<T> stringConverter) {
		bindBiDirectionalMapElement(propertyName, name, getMap, model -> stringConverter);
	}

	default <T extends Serializable> void bindBiDirectionalMapElement(String propertyName, String name, Function<Context, ObservableMap<String, String>> getMap, Function<Context, StringConverter<T>> getStringConverter) {
		addPrefixBinding(modelContext -> {
			ObservableMap<String, String> map = getMap.apply(modelContext);
			StringConverter<T> stringConverter = getStringConverter.apply(modelContext);
			ChangeListener<T> listener = (o, old, newValue) -> map.put(name, stringConverter.toString(newValue));
			Property<T> observable = getProperty(propertyName, modelContext);
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

	@Override
	default <T extends Serializable> void addPropertyChangeListener(String propertyName, BiConsumer<Context, T> listener) {
		addPrefixBinding(modelContext -> {
			ObservableValue<T> observable = getObservableValue(propertyName, modelContext);
			observable.addListener((o, old, nva) -> listener.accept(modelContext, nva));
		});
	}

	@Override
	default void createNewProperty(String propertyName) {
		addPrefixBinding(modelContext -> modelContext.createNewProperty(this, propertyName));
	}

	@Override
	default <T> void createNewInitializedProperty(String propertyName, Context model, Function<Context, T> getInitialValue) {
		model.createNewProperty(this, propertyName);
		getProperty(propertyName, model).setValue(getInitialValue.apply(model));
	}

	@Override
	default <T> void createNewInitializedProperty(String propertyName, Function<Context, T> getInitialValue) {
		createNewProperty(propertyName);
		initProperty(propertyName, getInitialValue);
	}

	@Override
	default <T> void initProperty(String propertyName, Function<Context, T> getInitialValue) {
		addPrefixBinding(modelContext -> getProperty(propertyName, modelContext).setValue(getInitialValue.apply(modelContext)));
	}

	@Override
	default <T> void storeProperty(String propertyName, Function<Context, ObservableValue<T>> applyOnModel) {
		addPrefixBinding(modelContext -> modelContext.storeProperty(this, propertyName, applyOnModel.apply(modelContext)));
	}

	@Override
	default <T> void storeProperty(String propertyName, Context model, Function<Context, ObservableValue<T>> applyOnModel) {
		model.storeProperty(this, propertyName, applyOnModel.apply(model));
	}

	default void addStyle(String propertyName, String value) {
		addPrefixBinding(model -> getDomNodeStyles(model).put(propertyName, value));
	}

	@Override
	default void addStyle(Context context, String propertyName, String value) {
		getDomNodeStyles(context).put(propertyName, value);
	}

	default void bindStyle(String style, String modelPropertyName) {
		bindMapElement(style, modelPropertyName, model -> getDomNodeStyles(model));
	}

	default void bindStyle(String style, String propertyName, Function<Context, ObservableValue<String>> applyOnModel) {
		storeProperty(propertyName, applyOnModel);
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

	default void bindAttribute(String attributeName, String propertyName) {
		bindMapElement(attributeName, propertyName, model -> getDomNodeAttributes(model));
	}

	default void bindAttribute(String attributeName, String propertyName, Function<Context, ObservableValue<String>> applyOnModel) {
		storeProperty(propertyName, applyOnModel);
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

	default HtmlDomNode createNode(HtmlDomNode parent, Context modelContext) {
		return new HtmlDomNode(parent, modelContext, this);
	};

	public ObservableList<Tag> getObservableChildren();

	default void init() {
	}

	default void beforeProcessAnnotations() {
	}

	public <COMPONENT extends Tag> COMPONENT getParent();

	default RootTag getRootTag() {
		return getParent().getRootTag();
	}

	public static interface RootTag extends Tag {
		default RootHtmlDomNode init(Context rootModelContext, String rootId, ServerWebSocket webSocket) {
			return new RootHtmlDomNode(rootModelContext, this, rootId, webSocket);
		}

		@Override
		default RootTag getRootTag() {
			return this;
		}

		public AnnotationsManager getAnnotationsManager();
	}

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
				if (!tagClass.equals(child.getClass()))
					System.out.println("Search : " + tagClass.getSimpleName() + " find polymorphic class : " + child.getClass().getSimpleName());
				if (posFound == pos)
					return (T) child;
				posFound++;
			}
		}
		throw new IllegalStateException("No tag corresponding to class " + tagClass.getSimpleName() + " found, position " + pos + ".");
	}

	default <T extends Tag> T createTag(Class<T> tagClass) {
		T result = null;
		try {
			result = tagClass.newInstance();
		} catch (IllegalAccessException | InstantiationException e) {
			throw new IllegalStateException(e);
		}
		((GSTagImpl) result).setParent(this);
		result.processAnnotations();
		result.init();
		return result;
	}

	default <T extends Tag> void processAnnotations() {
		getRootTag().getAnnotationsManager().processAnnotations(this);
	}

}
