package org.genericsystem.reactor;

import io.vertx.core.http.ServerWebSocket;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
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
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.modelproperties.AttributesDefaults;
import org.genericsystem.reactor.modelproperties.DisplayDefaults;
import org.genericsystem.reactor.modelproperties.GenericStringDefaults;
import org.genericsystem.reactor.modelproperties.StyleClassesDefaults;
import org.genericsystem.reactor.modelproperties.StylesDefaults;
import org.genericsystem.reactor.modelproperties.TextPropertyDefaults;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 */
public abstract class Tag implements TextPropertyDefaults, StylesDefaults, AttributesDefaults, StyleClassesDefaults, GenericStringDefaults, DisplayDefaults {

	private static final Logger log = LoggerFactory.getLogger(Tag.class);
	private final String tag;
	private MetaBinding<?> metaBinding;
	private final List<BiConsumer<Context, HtmlDomNode>> preFixedBindings = new ArrayList<>();
	private final List<BiConsumer<Context, HtmlDomNode>> postFixedBindings = new ArrayList<>();
	private final Tag parent;
	private final ObservableList<Tag> children = FXCollections.observableArrayList();

	@Override
	public String toString() {
		return tag + " " + getClass().getName();
	}

	protected Tag(Tag parent, String tag) {
		this.tag = tag;
		this.parent = parent;
		if (parent != null)
			parent.getObservableChildren().add(this);
	}

	public String getTag() {
		return tag;
	}

	protected List<BiConsumer<Context, HtmlDomNode>> getPreFixedBindings() {
		return preFixedBindings;
	}

	protected List<BiConsumer<Context, HtmlDomNode>> getPostFixedBindings() {
		return postFixedBindings;
	}

	@SuppressWarnings("unchecked")
	protected <BETWEEN> MetaBinding<BETWEEN> getMetaBinding() {
		return (MetaBinding<BETWEEN>) metaBinding;
	}

	protected <BETWEEN> void setMetaBinding(MetaBinding<BETWEEN> metaBinding) {
		if (this.metaBinding != null)
			throw new IllegalStateException("MetaBinding already defined");
		this.metaBinding = metaBinding;
	}

	@Override
	public void addPrefixBinding(Consumer<Context> consumer) {
		preFixedBindings.add((modelContext, node) -> consumer.accept(modelContext));
	}

	@Override
	public void addPostfixBinding(Consumer<Context> consumer) {
		postFixedBindings.add((modelContext, node) -> consumer.accept(modelContext));
	}

	public void bindOptionalStyleClass(String styleClass, String propertyName) {
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

	public void bindOptionalStyleClass(String styleClass, String modelPropertyName, Function<Context, ObservableValue<Boolean>> applyOnModel) {
		storeProperty(modelPropertyName, applyOnModel);
		bindOptionalStyleClass(styleClass, modelPropertyName);
	}

	protected void forEach2(Function<Context, ObservableList<Generic>> applyOnModel) {
		setMetaBinding(MetaBinding.forEachMetaBinding(applyOnModel));
	}

	public void forEach(ObservableListExtractor observableListExtractor) {
		forEach2(model -> observableListExtractor.apply(model.getGenerics()));
	}

	protected void forEach(Tag parent) {
		forEach(gs -> parent.getObservableListExtractor().apply(gs));
	}

	public void select(Function<Generic[], Generic> genericSupplier) {
		forEach((ObservableListExtractor) gs -> {
			Generic generic = genericSupplier.apply(gs);
			return generic != null ? FXCollections.singletonObservableList(generic) : FXCollections.emptyObservableList();
		});
	}

	protected void select_(Function<Context, ObservableList<Context>> applyOnModel) {
		setMetaBinding(MetaBinding.selectMetaBinding(applyOnModel));
	}

	public void select__(Function<Context, ObservableValue<Context>> applyOnModelContext) {
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

	// TODO try call select_(Function<Context, ObservableValue<Context>> applyOnModelContext) in place, transmitSuccessiveInvalidations will probably fix issues
	@Deprecated
	public void select(BiFunction<Context, ObservableList<Generic>, ObservableList<Context>> applyOnModel) {
		select_(model -> new ListBinding<Context>() {
			ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(model.getGenerics());
			{
				bind(holders);
			}

			@Override
			protected ObservableList<Context> computeValue() {
				return applyOnModel.apply(model, holders);
			}
		});
	}

	public void select(Class<?> genericClass) {
		forEach((ObservableListExtractor) gs -> FXCollections.singletonObservableList(gs[0].getRoot().find(genericClass)));
	}

	public ObservableListExtractor getObservableListExtractor() {
		return ObservableListExtractor.SUBINSTANCES;
	}

	@Override
	public <T> Property<T> getProperty(String propertyName, Context model) {
		return getProperty(propertyName, new Context[] { model });
	}

	private <T> Property<T> getProperty(String propertyName, Context[] models) {
		Tag tag = this;
		while (tag != null && models[0] != null) {
			if (models[0].containsProperty(tag, propertyName))
				return models[0].getProperty(tag, propertyName);
			if (tag.metaBinding != null && models[0].getHtmlDomNode(tag.getParent()) == null)
				models[0] = models[0].getParent();
			tag = tag.getParent();
		}
		return null;
	}

	@Override
	public <T> Property<T> getInheritedProperty(String propertyName, Context[] model, Tag[] tag) {
		while (tag != null && model[0] != null) {
			if (tag[0].metaBinding != null && model[0].getHtmlDomNode(tag[0].getParent()) == null)
				model[0] = model[0].getParent();
			tag[0] = tag[0].getParent();
			if (model[0] != null && model[0].containsProperty(tag[0], propertyName))
				return model[0].<T> getProperty(tag[0], propertyName);
		}
		return null;
	}

	@Override
	public <T> ObservableValue<T> getObservableValue(String propertyName, Context model) {
		return getObservableValue(propertyName, new Context[] { model });
	}

	private <T> ObservableValue<T> getObservableValue(String propertyName, Context[] model) {
		Tag tag = this;
		while (tag != null && model[0] != null) {
			if (model[0].containsProperty(tag, propertyName))
				return model[0].<T> getObservableValue(tag, propertyName);
			if (tag.metaBinding != null && model[0].getHtmlDomNode(tag.getParent()) == null)
				model[0] = model[0].getParent();
			tag = tag.getParent();
		}
		return null;
	}

	private void bindMapElement(String name, String propertyName, Function<Context, Map<String, String>> getMap) {
		addPrefixBinding(model -> {
			Map<String, String> map = getMap.apply(model);
			ChangeListener<String> listener = (o, old, newValue) -> map.put(name, newValue);
			ObservableValue<String> observable = getObservableValue(propertyName, model);
			observable.addListener(listener);
			map.put(name, observable.getValue());
		});
	}

	private void bindBiDirectionalMapElement(String propertyName, String name, Function<Context, ObservableMap<String, String>> getMap) {
		bindBiDirectionalMapElement(propertyName, name, getMap, ApiStatics.STRING_CONVERTERS.get(String.class));
	}

	private <T extends Serializable> void bindBiDirectionalMapElement(String propertyName, String name, Function<Context, ObservableMap<String, String>> getMap, StringConverter<T> stringConverter) {
		bindBiDirectionalMapElement(propertyName, name, getMap, model -> stringConverter);
	}

	private <T extends Serializable> void bindBiDirectionalMapElement(String propertyName, String name, Function<Context, ObservableMap<String, String>> getMap, Function<Context, StringConverter<T>> getStringConverter) {
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

	public <T extends Serializable> void addPropertyChangeListener(String propertyName, BiConsumer<Context, T> listener) {
		addPrefixBinding(modelContext -> {
			ObservableValue<T> observable = getObservableValue(propertyName, modelContext);
			observable.addListener((o, old, nva) -> listener.accept(modelContext, nva));
		});
	}

	@Override
	public void createNewProperty(String propertyName) {
		addPrefixBinding(modelContext -> modelContext.createNewProperty(this, propertyName));
	}

	@Override
	public <T> void createNewInitializedProperty(String propertyName, Context model, Function<Context, T> getInitialValue) {
		model.createNewProperty(this, propertyName);
		getProperty(propertyName, model).setValue(getInitialValue.apply(model));
	}

	@Override
	public <T> void createNewInitializedProperty(String propertyName, Function<Context, T> getInitialValue) {
		createNewProperty(propertyName);
		initProperty(propertyName, getInitialValue);
	}

	@Override
	public <T> void initProperty(String propertyName, Function<Context, T> getInitialValue) {
		addPrefixBinding(modelContext -> getProperty(propertyName, modelContext).setValue(getInitialValue.apply(modelContext)));
	}

	@Override
	public <T> void storeProperty(String propertyName, Function<Context, ObservableValue<T>> applyOnModel) {
		addPrefixBinding(modelContext -> modelContext.storeProperty(this, propertyName, applyOnModel.apply(modelContext)));
	}

	@Override
	public <T> void storeProperty(String propertyName, Context model, Function<Context, ObservableValue<T>> applyOnModel) {
		model.storeProperty(this, propertyName, applyOnModel.apply(model));
	}

	public void addStyle(String propertyName, String value) {
		addPrefixBinding(model -> getDomNodeStyles(model).put(propertyName, value));
	}

	public void bindStyle(String style, String modelPropertyName) {
		bindMapElement(style, modelPropertyName, model -> getDomNodeStyles(model));
	}

	public void bindStyle(String style, String propertyName, Function<Context, ObservableValue<String>> applyOnModel) {
		storeProperty(propertyName, applyOnModel);
		bindMapElement(style, propertyName, model -> getDomNodeStyles(model));
	}

	public void addStyleClasses(String... styleClasses) {
		addPrefixBinding(model -> getDomNodeStyleClasses(model).addAll(Arrays.asList(styleClasses)));
	}

	public void addStyleClass(String styleClass) {
		addPrefixBinding(model -> getDomNodeStyleClasses(model).add(styleClass));
	}

	public void addAttribute(String attributeName, String value) {
		addPrefixBinding(model -> getDomNodeAttributes(model).put(attributeName, value));
	}

	public void bindAttribute(String attributeName, String propertyName) {
		bindMapElement(attributeName, propertyName, model -> getDomNodeAttributes(model));
	}

	public void bindAttribute(String attributeName, String propertyName, Function<Context, ObservableValue<String>> applyOnModel) {
		storeProperty(propertyName, applyOnModel);
		bindMapElement(attributeName, propertyName, model -> getDomNodeAttributes(model));
	}

	public void bindBiDirectionalAttribute(String propertyName, String attributeName) {
		bindBiDirectionalMapElement(propertyName, attributeName, model -> getDomNodeAttributes(model));
	}

	public <T extends Serializable> void bindBiDirectionalAttribute(String propertyName, String attributeName, StringConverter<T> stringConverter) {
		bindBiDirectionalMapElement(propertyName, attributeName, model -> getDomNodeAttributes(model), stringConverter);
	}

	public <T extends Serializable> void bindBiDirectionalAttribute(String propertyName, String attributeName, Function<Context, StringConverter<T>> getStringConverter) {
		bindBiDirectionalMapElement(propertyName, attributeName, model -> getDomNodeAttributes(model), getStringConverter);
	}

	public void bindOptionalBiDirectionalAttribute(String propertyName, String attributeName, String attributeValue) {
		bindOptionalBiDirectionalAttribute(propertyName, attributeName, attributeValue, null);
	}

	public void bindOptionalBiDirectionalAttribute(String propertyName, String attributeName, String attributeValue, String attributeValueFalse) {
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

	protected HtmlDomNode createNode(HtmlDomNode parent, Context modelContext) {
		return new HtmlDomNode(parent, modelContext, this);
	};

	protected ObservableList<Tag> getObservableChildren() {
		return children;
	}

	@SuppressWarnings("unchecked")
	public <COMPONENT extends Tag> COMPONENT getParent() {
		return (COMPONENT) parent;
	}

	public static interface RootTag {
		default RootHtmlDomNode init(Context rootModelContext, String rootId, ServerWebSocket webSocket) {
			return new RootHtmlDomNode(rootModelContext, this, rootId, webSocket);
		}
	}
}
