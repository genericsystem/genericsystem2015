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

import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.MapChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.util.StringConverter;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.ViewContext.RootViewContext;
import org.genericsystem.reactor.html.TextPropertyDefaults;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 */
public abstract class Tag<M extends Model> implements TextPropertyDefaults<M> {

	private static final Logger log = LoggerFactory.getLogger(Tag.class);
	private final String tag;
	private BiConsumer<Tag<M>, ViewContext<M>> metaBinding;
	private final List<BiConsumer<Model, HtmlDomNode>> preFixedBindings = new ArrayList<>();
	private final List<BiConsumer<Model, HtmlDomNode>> postFixedBindings = new ArrayList<>();
	private final Tag parent;
	private final List<Tag<?>> children = new ArrayList<>();

	@Override
	public String toString() {
		return tag + " " + getClass().getName();
	}

	protected Tag(Tag<?> parent, String tag) {
		this.tag = tag;
		this.parent = parent;
		if (parent != null)
			parent.getChildren().add(this);
	}

	public String getTag() {
		return tag;
	}

	protected List<BiConsumer<Model, HtmlDomNode>> getPreFixedBindings() {
		return preFixedBindings;
	}

	protected List<BiConsumer<Model, HtmlDomNode>> getPostFixedBindings() {
		return postFixedBindings;
	}

	protected BiConsumer<Tag<M>, ViewContext<M>> getMetaBinding() {
		return metaBinding;
	}

	protected void setMetaBinding(BiConsumer<Tag<M>, ViewContext<M>> metaBinding) {
		if (this.metaBinding != null)
			throw new IllegalStateException("MetaBinding already defined");
		this.metaBinding = metaBinding;
	}

	@Override
	public void addPrefixBinding(Consumer<M> consumer) {
		preFixedBindings.add((modelContext, node) -> consumer.accept((M) modelContext));
	}

	public void addPostfixBinding(Consumer<M> consumer) {
		postFixedBindings.add((modelContext, node) -> consumer.accept((M) modelContext));
	}

	protected <NODE extends HtmlDomNode> void addActionBinding(Function<NODE, Property<Consumer<Object>>> applyOnNode, Consumer<M> applyOnModel) {
		preFixedBindings.add((modelContext, node) -> applyOnNode.apply((NODE) node).setValue(o -> applyOnModel.accept((M) modelContext)));
	}

	public void bindOptionalStyleClass(String styleClass, String propertyName) {
		addPrefixBinding(modelContext -> {
			ObservableValue<Boolean> optional = getObservableValue(propertyName, modelContext);
			Set<String> styleClasses = modelContext.getObservableStyleClasses(this);
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

	public void bindOptionalStyleClass(String styleClass, String modelPropertyName, Function<M, ObservableValue<Boolean>> applyOnModel) {
		storeProperty(modelPropertyName, applyOnModel);
		bindOptionalStyleClass(styleClass, modelPropertyName);
	}

	public <MODEL extends Model> void forEach(Function<MODEL, ObservableList<M>> applyOnModel) {
		forEach(applyOnModel, (model, subElement) -> {
			subElement.parent = model;
			return subElement;
		});
	}

	protected <MODEL extends Model, SUBELEMENT> void forEach(Function<MODEL, ObservableList<SUBELEMENT>> applyOnModel, BiFunction<MODEL, SUBELEMENT, M> modelBuilder) {
		setMetaBinding((childElement, viewContext) -> {
			MODEL model = viewContext.getModelContext();
			ObservableList<SUBELEMENT> subElements = applyOnModel.apply(model);
			ObservableList<M> subModels = new TransformationObservableList<SUBELEMENT, M>(subElements, (index, subModel) -> {
				M resultModel = modelBuilder.apply(model, subModel);
				viewContext.createViewContextChild(index, resultModel, childElement);
				return resultModel;
			}, Model::destroy);
			setSubModels(model, childElement, subModels);
		});
	}

	protected <SUBMODEL extends Model> void setSubModels(Model model, Tag<?> child, ObservableList<SUBMODEL> subModels) {
		model.setSubContexts(child, subModels);
	}

	@Override
	public <T> Property<T> getProperty(String propertyName, Model model) {
		return getProperty(propertyName, new Model[] { model });
	}

	private <T> Property<T> getProperty(String propertyName, Model[] model) {
		Tag<?> tag = this;
		while (tag != null && model[0] != null) {
			if (model[0].containsProperty(tag, propertyName))
				return model[0].getProperty(tag, propertyName);
			if (tag.metaBinding != null && model[0].getViewContext(tag.getParent()) == null)
				model[0] = model[0].getParent();
			tag = tag.getParent();
		}
		return null;
	}

	public <T> ObservableValue<T> getObservableValue(String propertyName, Model model) {
		return getObservableValue(propertyName, new Model[] { model });
	}

	private <T> ObservableValue<T> getObservableValue(String propertyName, Model[] model) {
		Tag<?> tag = this;
		while (tag != null && model[0] != null) {
			if (model[0].containsProperty(tag, propertyName))
				return model[0].getObservableValue(tag, propertyName);
			if (tag.metaBinding != null && model[0].getViewContext(tag.getParent()) == null)
				model[0] = model[0].getParent();
			tag = tag.getParent();
		}
		return null;
	}

	public void addSelectionIndex(int value) {
		addPrefixBinding(modelContext -> modelContext.getSelectionIndex(this).setValue(value));
	}

	private void bindMapElement(String name, String propertyName, Function<Model, Map<String, String>> getMap) {
		addPrefixBinding(model -> {
			Map<String, String> map = getMap.apply(model);
			ChangeListener<String> listener = (o, old, newValue) -> map.put(name, newValue);
			ObservableValue<String> observable = getObservableValue(propertyName, model);
			observable.addListener(listener);
			map.put(name, observable.getValue());
		});
	}

	private void bindBiDirectionalMapElement(String propertyName, String name, Function<Model, ObservableMap<String, String>> getMap) {
		bindBiDirectionalMapElement(propertyName, name, getMap, ApiStatics.STRING_CONVERTERS.get(String.class));
	}

	private <T extends Serializable> void bindBiDirectionalMapElement(String propertyName, String name, Function<Model, ObservableMap<String, String>> getMap, StringConverter<T> stringConverter) {
		bindBiDirectionalMapElement(propertyName, name, getMap, model -> stringConverter);
	}

	private <T extends Serializable> void bindBiDirectionalMapElement(String propertyName, String name, Function<Model, ObservableMap<String, String>> getMap, Function<M, StringConverter<T>> getStringConverter) {
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

	public <T extends Serializable> void addPropertyChangeListener(String propertyName, BiConsumer<M, T> listener) {
		addPrefixBinding(modelContext -> {
			ObservableValue<T> observable = getObservableValue(propertyName, modelContext);
			observable.addListener((o, old, nva) -> listener.accept(modelContext, nva));
		});
	}

	public void createNewProperty(String propertyName) {
		addPrefixBinding(modelContext -> modelContext.createNewProperty(this, propertyName));
	}

	public <T> void createNewInitializedProperty(String propertyName, Function<M, T> getInitialValue) {
		createNewProperty(propertyName);
		initProperty(propertyName, getInitialValue);
	}

	public <T> void initProperty(String propertyName, Function<M, T> getInitialValue) {
		addPrefixBinding(modelContext -> getProperty(propertyName, modelContext).setValue(getInitialValue.apply(modelContext)));
	}

	public <T> void storeProperty(String propertyName, Function<M, ObservableValue<T>> applyOnModel) {
		addPrefixBinding(modelContext -> modelContext.storeProperty(this, propertyName, applyOnModel.apply(modelContext)));
	}

	@Override
	public <T> void storePropertyWithoutCheck(String propertyName, M model, Function<M, ObservableValue<T>> applyOnModel) {
		model.storePropertyWithoutCheck(this, propertyName, applyOnModel.apply(model));
	}

	public void addStyle(String propertyName, String value) {
		addPrefixBinding(model -> model.getObservableStyles(this).put(propertyName, value));
	}

	public void bindStyle(String style, String modelPropertyName) {
		bindMapElement(style, modelPropertyName, model -> model.getObservableStyles(this));
	}

	public void bindStyle(String style, String propertyName, Function<M, ObservableValue<String>> applyOnModel) {
		storeProperty(propertyName, applyOnModel);
		bindMapElement(style, propertyName, model -> model.getObservableStyles(this));
	}

	public void addStyleClasses(String... styleClasses) {
		addPrefixBinding(model -> model.getObservableStyleClasses(this).addAll(Arrays.asList(styleClasses)));
	}

	public void addStyleClass(String styleClass) {
		addPrefixBinding(model -> model.getObservableStyleClasses(this).add(styleClass));
	}

	public void addAttribute(String attributeName, String value) {
		addPrefixBinding(model -> model.getObservableAttributes(this).put(attributeName, value));
	}

	public void bindAttribute(String attributeName, String propertyName) {
		bindMapElement(attributeName, propertyName, model -> model.getObservableAttributes(this));
	}

	public void bindAttribute(String attributeName, String propertyName, Function<M, ObservableValue<String>> applyOnModel) {
		storeProperty(propertyName, applyOnModel);
		bindMapElement(attributeName, propertyName, model -> model.getObservableAttributes(this));
	}

	public void bindBiDirectionalAttribute(String propertyName, String attributeName) {
		bindBiDirectionalMapElement(propertyName, attributeName, model -> model.getObservableAttributes(this));
	}

	public <T extends Serializable> void bindBiDirectionalAttribute(String propertyName, String attributeName, StringConverter<T> stringConverter) {
		bindBiDirectionalMapElement(propertyName, attributeName, model -> model.getObservableAttributes(this), stringConverter);
	}

	public <T extends Serializable> void bindBiDirectionalAttribute(String propertyName, String attributeName, Function<M, StringConverter<T>> getStringConverter) {
		bindBiDirectionalMapElement(propertyName, attributeName, model -> model.getObservableAttributes(this), getStringConverter);
	}

	public void bindOptionalBiDirectionalAttribute(String propertyName, String attributeName, String attributeValue) {
		bindOptionalBiDirectionalAttribute(propertyName, attributeName, attributeValue, null);
	}

	public void bindOptionalBiDirectionalAttribute(String propertyName, String attributeName, String attributeValue, String attributeValueFalse) {
		bindBiDirectionalMapElement(propertyName, attributeName, model -> model.getObservableAttributes(this), new StringConverter<Boolean>() {

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

	@Override
	public ViewContext<M> getViewContext(M model) {
		return model.getViewContext(this);
	}

	protected abstract HtmlDomNode createNode(String parentId);

	protected List<Tag<?>> getChildren() {
		return children;
	}

	@SuppressWarnings("unchecked")
	public <COMPONENT extends Tag<?>> COMPONENT getParent() {
		return (COMPONENT) parent;
	}

	public static interface RootTag<M extends Model> {
		default RootViewContext<M> init(M rootModelContext, String rootId, ServerWebSocket webSocket) {
			return new RootViewContext<M>(rootModelContext, this, rootId, webSocket);
		}
	}
}
