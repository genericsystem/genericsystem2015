package org.genericsystem.reactor;

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

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.reactor.ViewContext.RootViewContext;
import org.genericsystem.reactor.modelproperties.AttributesDefaults;
import org.genericsystem.reactor.modelproperties.StylesDefaults;
import org.genericsystem.reactor.modelproperties.TextPropertyDefaults;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.http.ServerWebSocket;
import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.MapChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.util.StringConverter;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 */
public abstract class Tag<M extends Model> implements TextPropertyDefaults<M>, StylesDefaults<M>, AttributesDefaults<M> {

	private static final Logger log = LoggerFactory.getLogger(Tag.class);
	private final String tag;
	private Function<Model, ObservableList<?>> metaBinding;
	private BiFunction<Model, ?, M> modelBuilder;
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

	protected Function<Model, ObservableList<?>> getMetaBinding() {
		return metaBinding;
	}

	protected BiFunction<Model, ?, M> getModelBuilder() {
		return modelBuilder;
	}

	protected void setMetaBinding(Function<Model, ObservableList<?>> metaBinding) {
		if (this.metaBinding != null)
			throw new IllegalStateException("MetaBinding already defined");
		this.metaBinding = metaBinding;
	}

	@Override
	public void addPrefixBinding(Consumer<M> consumer) {
		preFixedBindings.add((modelContext, node) -> consumer.accept((M) modelContext));
	}

	@Override
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

	protected <SUBELEMENT> void forEach(Function<Model, ObservableList<?>> applyOnModel, BiFunction<Model, SUBELEMENT, M> modelBuilder) {
		setMetaBinding(applyOnModel);
		this.modelBuilder = modelBuilder;
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

	@Override
	public <T> Property<T> getInheritedProperty(String propertyName, Model[] model, Tag<?>[] tag) {
		while (tag != null && model[0] != null) {
			if (tag[0].metaBinding != null && model[0].getViewContext(tag[0].getParent()) == null)
				model[0] = model[0].getParent();
			tag[0] = tag[0].getParent();
			if (model[0] != null && model[0].containsProperty(tag[0], propertyName))
				return model[0].getProperty(tag[0], propertyName);
		}
		return null;
	}

	@Override
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

	@Override
	public void createNewProperty(String propertyName) {
		addPrefixBinding(modelContext -> modelContext.createNewProperty(this, propertyName));
	}

	@Override
	public <T> void createNewInitializedProperty(String propertyName, M model, Function<M, T> getInitialValue) {
		model.createNewProperty(this, propertyName);
		getProperty(propertyName, model).setValue(getInitialValue.apply(model));
	}

	public <T> void createNewInitializedProperty(String propertyName, Function<M, T> getInitialValue) {
		createNewProperty(propertyName);
		initProperty(propertyName, getInitialValue);
	}

	public <T> void initProperty(String propertyName, Function<M, T> getInitialValue) {
		addPrefixBinding(modelContext -> getProperty(propertyName, modelContext).setValue(getInitialValue.apply(modelContext)));
	}

	@Override
	public <T> void storeProperty(String propertyName, Function<M, ObservableValue<T>> applyOnModel) {
		addPrefixBinding(modelContext -> modelContext.storeProperty(this, propertyName, applyOnModel.apply(modelContext)));
	}

	@Override
	public <T> void storeProperty(String propertyName, M model, Function<M, ObservableValue<T>> applyOnModel) {
		model.storeProperty(this, propertyName, applyOnModel.apply(model));
	}

	public void addStyle(String propertyName, String value) {
		addPrefixBinding(model -> getDomNodeStyles(model).put(propertyName, value));
	}

	public void bindStyle(String style, String modelPropertyName) {
		bindMapElement(style, modelPropertyName, model -> getDomNodeStyles(model));
	}

	public void bindStyle(String style, String propertyName, Function<M, ObservableValue<String>> applyOnModel) {
		storeProperty(propertyName, applyOnModel);
		bindMapElement(style, propertyName, model -> getDomNodeStyles(model));
	}

	public void addStyleClasses(String... styleClasses) {
		addPrefixBinding(model -> model.getObservableStyleClasses(this).addAll(Arrays.asList(styleClasses)));
	}

	public void addStyleClass(String styleClass) {
		addPrefixBinding(model -> model.getObservableStyleClasses(this).add(styleClass));
	}

	public void addAttribute(String attributeName, String value) {
		addPrefixBinding(model -> getDomNodeAttributes(model).put(attributeName, value));
	}

	public void bindAttribute(String attributeName, String propertyName) {
		bindMapElement(attributeName, propertyName, model -> getDomNodeAttributes(model));
	}

	public void bindAttribute(String attributeName, String propertyName, Function<M, ObservableValue<String>> applyOnModel) {
		storeProperty(propertyName, applyOnModel);
		bindMapElement(attributeName, propertyName, model -> getDomNodeAttributes(model));
	}

	public void bindBiDirectionalAttribute(String propertyName, String attributeName) {
		bindBiDirectionalMapElement(propertyName, attributeName, model -> getDomNodeAttributes(model));
	}

	public <T extends Serializable> void bindBiDirectionalAttribute(String propertyName, String attributeName, StringConverter<T> stringConverter) {
		bindBiDirectionalMapElement(propertyName, attributeName, model -> getDomNodeAttributes(model), stringConverter);
	}

	public <T extends Serializable> void bindBiDirectionalAttribute(String propertyName, String attributeName, Function<M, StringConverter<T>> getStringConverter) {
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
