package org.genericsystem.reactor;

import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.beans.value.WeakChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.MapChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.collections.ObservableSet;
import javafx.collections.SetChangeListener;
import javafx.collections.WeakMapChangeListener;
import javafx.collections.WeakSetChangeListener;
import javafx.util.StringConverter;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BidirectionalBinding;
import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.Model.TriFunction;
import org.genericsystem.reactor.composite.CompositeTag;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 */
public abstract class Tag<M extends Model> {

	private static final Logger log = LoggerFactory.getLogger(Tag.class);
	private final String tag;
	public BiConsumer<Tag<M>, ViewContext<?>> metaBinding;
	public final List<BiConsumer<Model, HtmlDomNode>> preFixedBindings = new ArrayList<>();
	public final List<BiConsumer<Model, HtmlDomNode>> postFixedBindings = new ArrayList<>();
	private final Tag<?> parent;
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

	public ServerWebSocket getWebSocket() {
		return getParent().getWebSocket();
	}

	protected <W, NODE extends HtmlDomNode> void addBidirectionalBinding(Function<NODE, Property<W>> applyOnNode, Function<M, Property<W>> applyOnModel) {
		preFixedBindings.add((modelContext, node) -> applyOnNode.apply((NODE) node).bindBidirectional(applyOnModel.apply((M) modelContext)));
	}

	public void addPrefixBinding(Consumer<M> consumer) {
		preFixedBindings.add((modelContext, node) -> consumer.accept((M) modelContext));
	}

	protected void addPostfixBinding(Consumer<M> consumer) {
		postFixedBindings.add((modelContext, node) -> consumer.accept((M) modelContext));
	}

	protected <NODE extends HtmlDomNode> void addActionBinding(Function<NODE, Property<Consumer<Object>>> applyOnNode, Consumer<M> applyOnModel) {
		preFixedBindings.add((modelContext, node) -> applyOnNode.apply((NODE) node).setValue(o -> applyOnModel.accept((M) modelContext)));
	}

	public <NODE extends HtmlDomNode> void bindOptionalStyleClass(String propertyName, String styleClass) {
		bindOptionalStyleClass(model -> model.getObservableValue(this, propertyName), styleClass);
	}

	public <NODE extends HtmlDomNode> void bindOptionalStyleClass(Function<M, ObservableValue<Boolean>> applyOnModel, String styleClass) {
		addPrefixBinding(modelContext -> {
			ObservableValue<Boolean> optional = applyOnModel.apply(modelContext);
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

	protected void forEach(CompositeTag<?> parentCompositeElement) {
		forEach(g -> parentCompositeElement.getStringExtractor().apply(g), gs -> parentCompositeElement.getObservableListExtractor().apply(gs));
	}

	public void forEach(ObservableListExtractor observableListExtractor) {
		forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, observableListExtractor);
	}

	public <MODEL extends Model> void forEach(Function<MODEL, ObservableList<M>> applyOnModel) {
		metaBinding = (childElement, viewContext) -> {
			M model = (M) viewContext.getModelContext();
			ObservableList<M> models = applyOnModel.apply((MODEL) model);
			viewContext.getModelContext().setSubContexts(childElement, new TransformationObservableList<M, MODEL>(models, (index, subModel) -> {
				subModel.parent = model;
				viewContext.createViewContextChild(index, subModel, childElement);
				return (MODEL) subModel;
			}, Model::destroy));
		};
	}

	public void forEach(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor) {
		metaBinding = (childElement, viewContext) -> {
			GenericModel model = (GenericModel) viewContext.getModelContext();
			ObservableList<Generic> generics = observableListExtractor.apply(model.getGenerics());
			viewContext.getModelContext().setSubContexts(childElement, new TransformationObservableList<Generic, GenericModel>(generics, (index, generic) -> {
				// System.out.println("Change detected on : " + System.identityHashCode(generics) + " newValue : " + generic.info());
					GenericModel duplicate = new GenericModel(model, GenericModel.addToGenerics(generic, model.getGenerics()), stringExtractor);
					viewContext.createViewContextChild(index, duplicate, childElement);
					return duplicate;
				}, m -> {
					// TODO unregister viewContext before removing in list ?
					m.destroy();
				}));
		};
	}

	public <MODEL extends GenericModel> void select_(Function<MODEL, ObservableValue<M>> applyOnModel) {
		select_(null, applyOnModel);
	}

	public <MODEL extends GenericModel> void select_(StringExtractor stringExtractor, Function<MODEL, ObservableValue<M>> applyOnModelContext) {
		metaBinding = (childElement, viewContext) -> {

			GenericModel model = (GenericModel) viewContext.getModelContext();
			ObservableValue<M> observableValue = applyOnModelContext.apply((MODEL) viewContext.getModelContext());
			ObservableList<M> subModels = FXCollections.observableArrayList();
			ChangeListener<M> listener = (ChangeListener<M>) (observable, oldValue, newValue) -> {
				if (oldValue != null)
					subModels.remove(0);
				if (newValue != null)
					subModels.add(newValue);
			};
			observableValue.addListener(listener);
			listener.changed(observableValue, null, observableValue.getValue());
			viewContext.getModelContext().setSubContexts(
					childElement,
					new TransformationObservableList<M, MODEL>(subModels, (index, selectedModel) -> {
						Generic[] gs = ((GenericModel) selectedModel).getGenerics();
						// assert Arrays.equals(gs, gs2) : Arrays.toString(gs) + " vs " + Arrays.toString(gs2);
							GenericModel childModel = new GenericModel(viewContext.getModelContext(), gs, stringExtractor != null ? stringExtractor
									: ((GenericModel) selectedModel).getStringExtractor());
							viewContext.createViewContextChild(index, childModel, childElement);
							return (MODEL) childModel;
						}, Model::destroy));
		};
	}

	public void select(StringExtractor stringExtractor, Function<Generic[], Generic> genericSupplier) {
		forEach(stringExtractor, gs -> {
			Generic generic = genericSupplier.apply(gs);
			return generic != null ? FXCollections.singletonObservableList(generic) : FXCollections.emptyObservableList();
		});
	}

	public void select(StringExtractor stringExtractor, Class<?> genericClass) {
		forEach(stringExtractor, gs -> FXCollections.singletonObservableList(gs[0].getRoot().find(genericClass)));
	}

	public void select(Function<Generic[], Generic> genericSupplier) {
		select(StringExtractor.SIMPLE_CLASS_EXTRACTOR, genericSupplier);
	}

	@FunctionalInterface
	public interface ModelConstructor<M extends Model> {
		M build(Generic[] generics, StringExtractor stringExtractor);
	}

	public void select(Class<?> genericClass) {
		select(StringExtractor.SIMPLE_CLASS_EXTRACTOR, genericClass);
	}

	public void addSelectionIndex(int value) {
		addPrefixBinding(modelContext -> modelContext.getSelectionIndex(this).setValue(value));
	}

	// public void bindSelectionIndex(Function<M, ObservableValue<Number>> applyOnModel) {
	// addPrefixBinding(modelContext -> modelContext.getSelectionIndex(this).bind(applyOnModel.apply(modelContext)));
	// }

	public void bindBidirectionalSelectionIndex(Function<M, Property<Number>> applyOnModel) {
		addPrefixBinding(modelContext -> modelContext.getSelectionIndex(this).bindBidirectional(applyOnModel.apply(modelContext)));
	}

	public <SUBMODEL extends GenericModel> void bindBiDirectionalSelection(Tag<SUBMODEL> subElement) {
		bindBiDirectionalSelection(subElement, 0);
	}

	protected <SUBMODEL extends GenericModel> void bindBiDirectionalSelection(Tag<SUBMODEL> subElement,
			Function<GenericModel, Property<GenericModel>> applyOnModel) {
		bindBiDirectionalSelection(subElement, applyOnModel, 0);
	}

	public <SUBMODEL extends GenericModel> void bindBiDirectionalSelection(Tag<SUBMODEL> subElement, int shift) {
		bindBiDirectionalSelection(subElement, GenericModel::getSelection, shift);
	}

	protected <SUBMODEL extends GenericModel> void bindBiDirectionalSelection(Tag<SUBMODEL> subElement,
			Function<GenericModel, Property<GenericModel>> applyOnModel, int shift) {
		addPostfixBinding(modelContext -> {
			List<SUBMODEL> subContexts = modelContext.getSubContexts(subElement);
			Generic selectedGeneric = ((GenericModel) modelContext).getGeneric();
			Optional<SUBMODEL> selectedModel = subContexts.stream().filter(sub -> selectedGeneric.equals(sub.getGeneric())).findFirst();
			Property<GenericModel> selection = applyOnModel.apply((GenericModel) modelContext);
			selection.setValue(selectedModel.isPresent() ? selectedModel.get() : null);
			BidirectionalBinding.bind(modelContext.getSelectionIndex(this), selection,
					number -> number.intValue() - shift >= 0 ? (GenericModel) subContexts.get(number.intValue() - shift) : null,
					genericModel -> subContexts.indexOf(genericModel) + shift);
		});
	}

	protected void enableSelectorBehavior() {
		addPrefixBinding(model -> ((GenericModel) model).enableSelectorBehavior());
	}

	private void bindMapElement(String name, String propertyName, Function<Model, Map<String, String>> getMap) {
		bindMapElement(name, model -> model.getObservableValue(this, propertyName), getMap);
	}

	private void bindMapElement(String name, Function<M, ObservableValue<String>> applyOnModel, Function<Model, Map<String, String>> getMap) {
		addPrefixBinding(modelContext -> {
			Map<String, String> map = getMap.apply(modelContext);
			ChangeListener<String> listener = (o, old, newValue) -> map.put(name, newValue);
			ObservableValue<String> observable = applyOnModel.apply(modelContext);
			observable.addListener(listener);
			map.put(name, observable.getValue());
		});
	}

	private void bindBiDirectionalMapElement(String propertyName, String name, Function<Model, ObservableMap<String, String>> getMap) {
		bindBiDirectionalMapElement(propertyName, name, getMap, ApiStatics.STRING_CONVERTERS.get(String.class));
	}

	private void bindBiDirectionalMapElement(Function<M, Property<String>> applyOnModel, String name, Function<Model, ObservableMap<String, String>> getMap) {
		bindBiDirectionalMapElement(applyOnModel, name, getMap, ApiStatics.STRING_CONVERTERS.get(String.class));
	}

	private <T extends Serializable> void bindBiDirectionalMapElement(String propertyName, String name, Function<Model, ObservableMap<String, String>> getMap,
			StringConverter<T> stringConverter) {
		bindBiDirectionalMapElement(model -> model.getProperty(this, propertyName), name, getMap, stringConverter);
	}

	private <T extends Serializable> void bindBiDirectionalMapElement(Function<M, Property<T>> applyOnModel, String name,
			Function<Model, ObservableMap<String, String>> getMap, StringConverter<T> stringConverter) {
		bindBiDirectionalMapElement(applyOnModel, name, getMap, model -> stringConverter);
	}

	private <T extends Serializable> void bindBiDirectionalMapElement(String propertyName, String name, Function<Model, ObservableMap<String, String>> getMap,
			Function<M, StringConverter<T>> getStringConverter) {
		bindBiDirectionalMapElement(model -> model.getProperty(this, propertyName), name, getMap, getStringConverter);
	}

	private <T extends Serializable> void bindBiDirectionalMapElement(Function<M, Property<T>> applyOnModel, String name,
			Function<Model, ObservableMap<String, String>> getMap, Function<M, StringConverter<T>> getStringConverter) {
		addPrefixBinding(modelContext -> {
			ObservableMap<String, String> map = getMap.apply(modelContext);
			StringConverter<T> stringConverter = getStringConverter.apply(modelContext);
			ChangeListener<T> listener = (o, old, newValue) -> map.put(name, stringConverter.toString(newValue));
			Property<T> observable = applyOnModel.apply(modelContext);
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

	public <T extends Serializable> void bindOperation(String propertyName, BiConsumer<M, T> operation) {
		bindOperation(model -> model.getProperty(this, propertyName), operation);
	}

	public <T extends Serializable> void bindOperation(Function<M, Property<T>> applyOnModel, BiConsumer<M, T> operation) {
		addPrefixBinding(modelContext -> {
			Property<T> observable = applyOnModel.apply(modelContext);
			observable.addListener((o, old, nva) -> operation.accept(modelContext, nva));
		});
	}

	public <T> void initProperty(String propertyName, Function<M, T> getInitialValue) {
		initProperty(model -> model.getProperty(this, propertyName), getInitialValue);
	}

	public <T> void initProperty(Function<M, Property<T>> applyOnModel, Function<M, T> getInitialValue) {
		addPrefixBinding(modelContext -> {
			Property<T> observable = applyOnModel.apply(modelContext);
			observable.setValue(getInitialValue.apply(modelContext));
		});
	}

	public <T> void setProperty(String propertyName, Function<M, ObservableValue<T>> applyOnModel) {
		addPrefixBinding(modelContext -> {
			modelContext.setProperty(this, propertyName, applyOnModel.apply(modelContext));
		});
	}

	public <T> void setAction(TriFunction<Generic[], Serializable, Generic, Generic> action) {
		addPrefixBinding(modelContext -> {
			modelContext.setAction(action);
		});
	}

	public void addStyle(String propertyName, String value) {
		addPrefixBinding(model -> model.getObservableStyles(this).put(propertyName, value));
	}

	public void bindOptionalStyle(String propertyName, String modelPropertyName, String propertyValue) {
		bindOptionalStyle(propertyName, modelPropertyName, propertyValue, "");
	}

	public void bindOptionalStyle(String propertyName, Function<M, ObservableValue<Boolean>> applyOnModel, String propertyValue) {
		bindOptionalStyle(propertyName, applyOnModel, propertyValue, "");
	}

	public void bindOptionalStyle(String propertyName, String modelPropertyName, String propertyValue, String propertyValueFalse) {
		bindOptionalStyle(propertyName, model -> model.getObservableValue(this, modelPropertyName), propertyValue, propertyValueFalse);
	}

	public void bindOptionalStyle(String propertyName, Function<M, ObservableValue<Boolean>> applyOnModel, String propertyValue, String propertyValueFalse) {
		bindStyle(propertyName, model -> {
			ObservableValue<Boolean> optional = applyOnModel.apply(model);
			return Bindings.createStringBinding(() -> Boolean.TRUE.equals(optional.getValue()) ? propertyValue : propertyValueFalse, optional);
		});
	}

	// public <SUBMODEL extends GenericModel> void bindOptionalStyle(String propertyName, Function<SUBMODEL, ObservableValue<Boolean>> applyOnModel,
	// Function<SUBMODEL, ObservableValue<String>> propertyValueGetter, String propertyValueFalse) {
	// bindStyle(propertyName, model -> {
	// ObservableValue<Boolean> optional = applyOnModel.apply((SUBMODEL) model);
	// return Bindings.createStringBinding(() -> optional.getValue() ? propertyValueGetter.apply((SUBMODEL) model).getValue() : propertyValueFalse,
	// optional);
	// });
	// }

	public void bindStyle(String propertyName, String modelPropertyName) {
		bindMapElement(propertyName, modelPropertyName, model -> model.getObservableStyles(this));
	}

	public void bindStyle(String propertyName, Function<M, ObservableValue<String>> applyOnModel) {
		bindMapElement(propertyName, applyOnModel, model -> model.getObservableStyles(this));
	}

	// public void setStyleClasses(Set<String> styleClasses) {
	// addPrefixBinding(model -> {
	// ObservableSet<String> observableStyleClasses = model.getObservableStyleClasses(this);
	// observableStyleClasses.removeIf(styleClass -> !styleClasses.contains(styleClass));
	// observableStyleClasses.addAll(styleClasses);
	// });
	// }

	public void addStyleClasses(String... styleClasses) {
		addPrefixBinding(model -> model.getObservableStyleClasses(this).addAll(Arrays.asList(styleClasses)));
	}

	// public void bindStyleClasses(ObservableSet<String> styleClasses) {
	// addPrefixBinding(model -> Bindings.bindContent(model.getObservableStyleClasses(this), styleClasses));
	// }
	//
	// public void bindStylesMap(ObservableMap<String, String> styles) {
	// addPrefixBinding(model -> Bindings.bindContent(model.getObservableStyles(this), styles));
	// }
	//
	// public void setStylesMap(Map<String, String> styles) {
	// addPrefixBinding(model -> model.getObservableStyles(this).putAll(styles));
	// }

	public void addStyleClass(String styleClass) {
		addPrefixBinding(model -> model.getObservableStyleClasses(this).add(styleClass));
	}

	public void addAttribute(String attributeName, String value) {
		addPrefixBinding(model -> model.getObservableAttributes(this).put(attributeName, value));
	}

	public void bindOptionalAttribute(String attributeName, String propertyName, String attributeValue) {
		bindOptionalAttribute(attributeName, propertyName, attributeValue, null);
	}

	public void bindOptionalAttribute(String attributeName, Function<M, ObservableValue<Boolean>> applyOnModel, String attributeValue) {
		bindOptionalAttribute(attributeName, applyOnModel, attributeValue, null);
	}

	public void bindOptionalAttribute(String attributeName, String propertyName, String attributeValue, String attributeValueFalse) {
		bindOptionalAttribute(attributeName, model -> model.getObservableValue(this, propertyName), attributeValue, attributeValueFalse);
	}

	public void bindOptionalAttribute(String attributeName, Function<M, ObservableValue<Boolean>> applyOnModel, String attributeValue,
			String attributeValueFalse) {
		bindAttribute(attributeName, model -> {
			ObservableValue<Boolean> optional = applyOnModel.apply(model);
			return Bindings.createStringBinding(() -> optional.getValue() ? attributeValue : attributeValueFalse, optional);
		});
	}

	public void bindAttribute(String attributeName, String propertyName) {
		bindMapElement(attributeName, propertyName, model -> model.getObservableAttributes(this));
	}

	public void bindAttribute(String attributeName, Function<M, ObservableValue<String>> applyOnModel) {
		bindMapElement(attributeName, applyOnModel, model -> model.getObservableAttributes(this));
	}

	public void bindBiDirectionalAttribute(String propertyName, String attributeName) {
		bindBiDirectionalMapElement(propertyName, attributeName, model -> model.getObservableAttributes(this));
	}

	public void bindBiDirectionalAttribute(Function<M, Property<String>> applyOnModel, String attributeName) {
		bindBiDirectionalMapElement(applyOnModel, attributeName, model -> model.getObservableAttributes(this));
	}

	public <T extends Serializable> void bindBiDirectionalAttribute(String propertyName, String attributeName, StringConverter<T> stringConverter) {
		bindBiDirectionalMapElement(propertyName, attributeName, model -> model.getObservableAttributes(this), stringConverter);
	}

	public <T extends Serializable> void bindBiDirectionalAttribute(Function<M, Property<T>> applyOnModel, String attributeName,
			StringConverter<T> stringConverter) {
		bindBiDirectionalMapElement(applyOnModel, attributeName, model -> model.getObservableAttributes(this), stringConverter);
	}

	public <T extends Serializable> void bindBiDirectionalAttribute(String propertyName, String attributeName,
			Function<M, StringConverter<T>> getStringConverter) {
		bindBiDirectionalMapElement(propertyName, attributeName, model -> model.getObservableAttributes(this), getStringConverter);
	}

	public <T extends Serializable> void bindBiDirectionalAttribute(Function<M, Property<T>> applyOnModel, String attributeName,
			Function<M, StringConverter<T>> getStringConverter) {
		bindBiDirectionalMapElement(applyOnModel, attributeName, model -> model.getObservableAttributes(this), getStringConverter);
	}

	public void bindOptionalBiDirectionalAttribute(String propertyName, String attributeName, String attributeValue) {
		bindOptionalBiDirectionalAttribute(propertyName, attributeName, attributeValue, null);
	}

	public void bindOptionalBiDirectionalAttribute(Function<M, Property<Boolean>> applyOnModel, String attributeName, String attributeValue) {
		bindOptionalBiDirectionalAttribute(applyOnModel, attributeName, attributeValue, null);
	}

	public void bindOptionalBiDirectionalAttribute(String propertyName, String attributeName, String attributeValue, String attributeValueFalse) {
		bindOptionalBiDirectionalAttribute(model -> model.getProperty(this, propertyName), attributeName, attributeValue, attributeValueFalse);
	}

	public void bindOptionalBiDirectionalAttribute(Function<M, Property<Boolean>> applyOnModel, String attributeName, String attributeValue,
			String attributeValueFalse) {
		bindBiDirectionalMapElement(applyOnModel, attributeName, model -> model.getObservableAttributes(this), new StringConverter<Boolean>() {

			@Override
			public String toString(Boolean bool) {
				return Boolean.TRUE.equals(bool) ? attributeValue : attributeValueFalse;
			}

			@Override
			public Boolean fromString(String string) {
				// return Objects.equals(attributeValue,string) instead ?
				return attributeValue.equals(string);
			}
		});
	}

	public void bindTextBidirectional(Function<M, Property<String>> applyOnModel) {
		addBidirectionalBinding(HtmlDomNode::getTextProperty, applyOnModel);
	}

	public void setText(String value) {
		addPrefixBinding(model -> model.getTextProperty(this).setValue(value));
	}

	public void bindText(Function<M, ObservableValue<String>> applyOnModel) {
		addPrefixBinding(modelContext -> modelContext.getTextProperty(this).bind(applyOnModel.apply(modelContext)));
	}

	protected abstract HtmlDomNode createNode(String parentId);

	protected List<Tag<?>> getChildren() {
		return children;
	}

	@SuppressWarnings("unchecked")
	public <COMPONENT extends Tag<?>> COMPONENT getParent() {
		return (COMPONENT) parent;
	}

	private static final String MSG_TYPE = "msgType";
	private static final String ADD = "A";
	private static final String UPDATE = "U";
	private static final String REMOVE = "R";
	private static final String UPDATE_TEXT = "UT";
	private static final String UPDATE_SELECTION = "US";
	private static final String ADD_STYLECLASS = "AC";
	private static final String REMOVE_STYLECLASS = "RC";
	private static final String ADD_STYLE = "AS";
	private static final String REMOVE_STYLE = "RS";
	private static final String ADD_ATTRIBUTE = "AA";
	private static final String REMOVE_ATTRIBUTE = "RA";

	private static final String PARENT_ID = "parentId";
	public static final String ID = "nodeId";
	private static final String NEXT_ID = "nextId";
	private static final String STYLE_PROPERTY = "styleProperty";
	private static final String STYLE_VALUE = "styleValue";
	private static final String ATTRIBUTE_NAME = "attributeName";
	private static final String ATTRIBUTE_VALUE = "attributeValue";
	private static final String STYLECLASS = "styleClass";
	private static final String TEXT_CONTENT = "textContent";
	private static final String TAG_HTML = "tagHtml";
	private static final String ELT_TYPE = "eltType";

	public class HtmlDomNode {

		private final String id;
		private final String parentId;
		private final StringProperty text = new SimpleStringProperty();
		private final ObservableSet<String> styleClasses = FXCollections.observableSet();
		private final ObservableMap<String, String> styles = FXCollections.observableHashMap();
		private final ObservableMap<String, String> attributes = FXCollections.observableHashMap();

		private final ChangeListener<String> textListener = (o, old, newValue) -> sendMessage(new JsonObject().put(MSG_TYPE, UPDATE_TEXT).put(ID, getId())
				.put(TEXT_CONTENT, newValue != null ? newValue : ""));

		private final MapChangeListener<String, String> stylesListener = change -> {
			if (!change.wasAdded() || change.getValueAdded() == null || change.getValueAdded().equals("")) {
				// System.out.println("Remove : " + change.getKey() + " " + change.getValueRemoved());
				sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_STYLE).put(ID, getId()).put(STYLE_PROPERTY, change.getKey()));
			} else if (change.wasAdded()) {
				// System.out.println("Add : " + change.getKey() + " " + change.getValueAdded());
				sendMessage(new JsonObject().put(MSG_TYPE, ADD_STYLE).put(ID, getId()).put(STYLE_PROPERTY, change.getKey())
						.put(STYLE_VALUE, change.getValueAdded()));
			}
		};

		private final MapChangeListener<String, String> attributesListener = change -> {
			if (!change.wasAdded() || change.getValueAdded() == null || change.getValueAdded().equals("")) {
				sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_ATTRIBUTE).put(ID, getId()).put(ATTRIBUTE_NAME, change.getKey()));
			} else if (change.wasAdded()) {
				sendMessage(new JsonObject().put(MSG_TYPE, ADD_ATTRIBUTE).put(ID, getId()).put(ATTRIBUTE_NAME, change.getKey())
						.put(ATTRIBUTE_VALUE, change.getValueAdded()));
			}
		};

		private final SetChangeListener<String> styleClassesListener = change -> {
			if (change.wasAdded()) {
				sendMessage(new JsonObject().put(MSG_TYPE, ADD_STYLECLASS).put(ID, getId()).put(STYLECLASS, change.getElementAdded()));
			} else {
				sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_STYLECLASS).put(ID, getId()).put(STYLECLASS, change.getElementRemoved()));
			}
		};

		public ObservableMap<String, String> getStyles() {
			return styles;
		}

		public ObservableMap<String, String> getAttributes() {
			return attributes;
		}

		public HtmlDomNode(String parentId) {
			this.parentId = parentId;
			this.id = String.format("%010d", Integer.parseInt(this.hashCode() + "")).substring(0, 10);
			text.addListener(new WeakChangeListener<>(textListener));
			styles.addListener(new WeakMapChangeListener<>(stylesListener));
			styleClasses.addListener(new WeakSetChangeListener<>(styleClassesListener));
			attributes.addListener(new WeakMapChangeListener<>(attributesListener));
		}

		public void sendAdd(int index) {
			JsonObject jsonObj = new JsonObject().put(MSG_TYPE, ADD);
			jsonObj.put(PARENT_ID, parentId);
			jsonObj.put(ID, id);
			jsonObj.put(TAG_HTML, getTag());
			jsonObj.put(NEXT_ID, index);
			fillJson(jsonObj);
			// System.out.println(jsonObj.encodePrettily());
			sendMessage(jsonObj);
		}

		public JsonObject fillJson(JsonObject jsonObj) {
			return null;
		}

		public void sendRemove() {
			sendMessage(new JsonObject().put(MSG_TYPE, REMOVE).put(ID, id));
			// System.out.println(new JsonObject().put(MSG_TYPE, REMOVE).put(ID, id).encodePrettily());
		}

		public void sendMessage(JsonObject jsonObj) {
			getWebSocket().writeFinalTextFrame(jsonObj.encode());
		}

		public ObservableSet<String> getStyleClasses() {
			return styleClasses;
		}

		public Property<String> getStyle(String propertyName) {
			Property<String> property = new SimpleStringProperty(styles.get(propertyName));
			property.addListener((c, o, n) -> styles.put(propertyName, n));
			return property;
		}

		public StringProperty getTextProperty() {
			return text;
		}

		public String getId() {
			return id;
		}

		public void handleMessage(JsonObject json) {

		}

	}

	public class ActionHtmlNode extends HtmlDomNode {
		public ActionHtmlNode(String parentId) {
			super(parentId);
		}

		private final Property<Consumer<Object>> actionProperty = new SimpleObjectProperty<>();

		public Property<Consumer<Object>> getActionProperty() {
			return actionProperty;
		}

		@Override
		public void handleMessage(JsonObject json) {
			getActionProperty().getValue().accept(new Object());
		}

	}

	public class SelectableHtmlDomNode extends ActionHtmlNode {
		private static final String SELECTED_INDEX = "selectedIndex";

		private Property<Number> selectionIndex = new SimpleIntegerProperty();

		private final ChangeListener<Number> indexListener = (o, old, newValue) -> {
			// System.out.println(new JsonObject().put(MSG_TYPE, UPDATE_SELECTION).put(ID, getId()).put(SELECTED_INDEX, newValue != null ? newValue : 0)
			// .encodePrettily());
			sendMessage(new JsonObject().put(MSG_TYPE, UPDATE_SELECTION).put(ID, getId()).put(SELECTED_INDEX, newValue != null ? newValue : 0));
		};

		public SelectableHtmlDomNode(String parentId) {
			super(parentId);
			selectionIndex.addListener(new WeakChangeListener<>(indexListener));
		}

		public Property<Number> getSelectionIndex() {
			return selectionIndex;
		}

		@Override
		public void handleMessage(JsonObject json) {
			if (UPDATE.equals(json.getString(MSG_TYPE))) {
				getSelectionIndex().setValue(json.getInteger(SELECTED_INDEX));
				// System.out.println("Selected index : " + getSelectionIndex().getValue());
			}
		}

	}

	public class InputTextHtmlDomNode extends HtmlDomNode {

		private final Property<String> inputString = new SimpleStringProperty();
		private final ObjectProperty<Consumer<Object>> enterProperty = new SimpleObjectProperty<>();

		public InputTextHtmlDomNode(String parentId) {
			super(parentId);
			inputString.addListener(new WeakChangeListener<>(inputListener));
		}

		private final ChangeListener<String> inputListener = (o, old, newValue) -> {
			assert old != newValue;
			System.out.println(new JsonObject().put(MSG_TYPE, UPDATE_TEXT).put(ID, getId()).encodePrettily());
			sendMessage(fillJson(new JsonObject().put(MSG_TYPE, UPDATE_TEXT).put(ID, getId())));
		};

		public Property<String> getInputString() {
			return inputString;
		}

		@Override
		public JsonObject fillJson(JsonObject jsonObj) {
			super.fillJson(jsonObj);
			return jsonObj.put("type", "text").put(TEXT_CONTENT, inputString.getValue());
		}

		@Override
		public void handleMessage(JsonObject json) {
			if (ADD.equals(json.getString(MSG_TYPE)))
				getEnterProperty().get().accept(new Object());
			if (UPDATE.equals(json.getString(MSG_TYPE))) {
				getTextProperty().setValue(json.getString(TEXT_CONTENT));
				getAttributes().put(ReactorStatics.VALUE, json.getString(TEXT_CONTENT));
			}
		}

		public ObjectProperty<Consumer<Object>> getEnterProperty() {
			return enterProperty;
		}

	}

	public class InputCheckHtmlDomNode extends HtmlDomNode {
		private final String type;

		public InputCheckHtmlDomNode(String parentId, String type) {
			super(parentId);
			this.type = type;
		}

		@Override
		public JsonObject fillJson(JsonObject jsonObj) {
			super.fillJson(jsonObj);
			return jsonObj.put("type", type);
		}

		@Override
		public void handleMessage(JsonObject json) {
			if ("checkbox".equals(json.getString(ELT_TYPE)))
				getAttributes().put(ReactorStatics.CHECKED, json.getBoolean(ReactorStatics.CHECKED) ? ReactorStatics.CHECKED : "");
		}
	}

}
