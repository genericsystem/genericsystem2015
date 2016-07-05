package org.genericsystem.reactor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.composite.CompositeTag;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.GenericModel.StringExtractor;
import org.genericsystem.reactor.model.InputGenericModel;
import org.genericsystem.reactor.model.InputGenericModel.TriFunction;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.SelectorModel;

import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;
import javafx.beans.binding.Bindings;
import javafx.beans.binding.ListBinding;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
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

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 */
public abstract class Tag<M extends Model> {

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

	protected void addPrefixBinding(Consumer<Model> consumer) {
		preFixedBindings.add((modelContext, node) -> consumer.accept(modelContext));
	}

	protected void addPostfixBinding(Consumer<Model> consumer) {
		postFixedBindings.add((modelContext, node) -> consumer.accept(modelContext));
	}

	protected <NODE extends HtmlDomNode> void addActionBinding(Function<NODE, Property<Consumer<Object>>> applyOnNode, Consumer<M> applyOnModel) {
		preFixedBindings.add((modelContext, node) -> applyOnNode.apply((NODE) node).setValue(o -> applyOnModel.accept((M) modelContext)));
	}

	protected <NODE extends HtmlDomNode> void addActionBinding2(Function<NODE, Property<Consumer<Object>>> applyOnNode,
			Consumer<Model> applyOnModelContext) {
		preFixedBindings.add((modelContext, node) -> applyOnNode.apply((NODE) node).setValue(o -> applyOnModelContext.accept(modelContext)));
	}

	public <NODE extends HtmlDomNode> void bindOptionalStyleClass(Function<M, ObservableValue<Boolean>> applyOnModel, String styleClass) {
		addPrefixBinding(modelContext -> {
			ObservableValue<Boolean> optional = applyOnModel.apply((M) modelContext);
			Set<String> styleClasses = modelContext.getObservableStyleClasses(this);
			Consumer<Boolean> consumer = bool -> {
				if (bool)
					styleClasses.add(styleClass);
				else
					styleClasses.remove(styleClass);
			};
			consumer.accept(optional.getValue());
			optional.addListener((o, ov, nv) -> consumer.accept(nv));
		});
	}

	public void select(StringExtractor stringExtractor, Function<Generic[], Generic> genericSupplier, ModelConstructor<GenericModel> constructor) {
		forEach(stringExtractor, gs -> {
			Generic generic = genericSupplier.apply(gs);
			return generic != null ? FXCollections.singletonObservableList(generic) : FXCollections.emptyObservableList();
		}, constructor);
	}

	public void select(StringExtractor stringExtractor, Class<?> genericClass, ModelConstructor<GenericModel> constructor) {
		forEach(stringExtractor, gs -> FXCollections.singletonObservableList(gs[0].getRoot().find(genericClass)), constructor);
	}

	public void forEach(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, ModelConstructor<GenericModel> constructor) {
		forEach(modelContext -> new TransformationObservableList<Generic, M>(observableListExtractor.apply(((GenericModel) modelContext).getGenerics()),
				generic -> (M) constructor.build(GenericModel.addToGenerics(generic, ((GenericModel) modelContext).getGenerics()), stringExtractor)));
	}

	protected <MODEL extends Model> void forEach(Function<MODEL, ObservableList<M>> applyOnModelContext) {
		forEach(applyOnModelContext, (modelContext, childModel) -> {
			childModel.parent = modelContext;
			return childModel;
		});
	}

	private <MODEL extends Model> void forEach(Function<MODEL, ObservableList<M>> applyOnModelContext,
			BiFunction<Model, MODEL, Model> modelContextBuilder) {
		metaBinding = (childElement, viewContext) -> viewContext.getModelContext().setSubContexts(childElement,
				new TransformationObservableList<M, MODEL>(applyOnModelContext.apply((MODEL) viewContext.getModelContext()), (index, childModel) -> {
					childModel.parent = viewContext.getModelContext();
					viewContext.createViewContextChild(index, childModel, childElement);
					return (MODEL) childModel;
				}, Model::destroy));
	}

	public <T> void select(Function<T, ObservableValue<M>> applyOnModel) {
		forEach(model -> {
			ObservableValue<M> observableValue = applyOnModel.apply((T) model);
			return new ListBinding<M>() {
				{
					bind(observableValue);
				}

				@Override
				protected ObservableList<M> computeValue() {
					M value = observableValue.getValue();
					return value != null ? FXCollections.singletonObservableList(value) : FXCollections.emptyObservableList();
				}
			};
		});
	}

	protected void forEach(CompositeTag<?> parentCompositeElement) {
		forEach(g -> parentCompositeElement.getStringExtractor().apply(g), gs -> parentCompositeElement.getObservableListExtractor().apply(gs),
				(gs, extractor) -> parentCompositeElement.getModelConstructor().build(gs, extractor));
	}

	public void forEach_(ObservableListExtractor observableListExtractor) {
		forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, observableListExtractor, GenericModel::new);
	}

	public void forEach(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor) {
		forEach(stringExtractor, observableListExtractor, GenericModel::new);
	}

	public void select(StringExtractor stringExtractor, Function<Generic[], Generic> genericSupplier) {
		select(stringExtractor, genericSupplier, GenericModel::new);
	}

	public void select(Function<Generic[], Generic> genericSupplier, ModelConstructor<GenericModel> constructor) {
		select(StringExtractor.SIMPLE_CLASS_EXTRACTOR, genericSupplier, constructor);
	}

	public void select_(Function<Generic[], Generic> genericSupplier) {
		select(StringExtractor.SIMPLE_CLASS_EXTRACTOR, genericSupplier, GenericModel::new);
	}

	public void select(StringExtractor stringExtractor, Class<?> genericClass) {
		select(stringExtractor, genericClass, GenericModel::new);
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

	public void bindSelectionIndex(Function<M, ObservableValue<Number>> applyOnModel) {
		addPrefixBinding(modelContext -> modelContext.getSelectionIndex(this).bind(applyOnModel.apply((M) modelContext)));
	}

	public void bindBidirectionalSelectionIndex(Function<M, Property<Number>> applyOnModel) {
		addPrefixBinding(modelContext -> modelContext.getSelectionIndex(this).bindBidirectional(applyOnModel.apply((M) modelContext)));
	}

	public <SUBMODEL extends GenericModel> void bindBiDirectionalSelection(Tag<SUBMODEL> subElement) {
		bindBiDirectionalSelection(subElement, 0);
	}

	public <SUBMODEL extends GenericModel> void bindBiDirectionalSelection(Tag<SUBMODEL> subElement, int shift) {
		bindBiDirectionalSelection(subElement, SelectorModel::getSelection, shift);
	}

	protected <SUBMODEL extends GenericModel> void bindBiDirectionalSelection(Tag<SUBMODEL> subElement,
			Function<SelectorModel, Property<GenericModel>> applyOnModel) {
		bindBiDirectionalSelection(subElement, applyOnModel, 0);
	}

	protected <SUBMODEL extends GenericModel> void bindBiDirectionalSelection(Tag<SUBMODEL> subElement,
			Function<SelectorModel, Property<GenericModel>> applyOnModel, int shift) {
		addPostfixBinding(modelContext -> {
			List<? extends Model> subContexts = modelContext.getSubContexts(subElement);
			BidirectionalBinding.bind(modelContext.getSelectionIndex(this), applyOnModel.apply((SelectorModel) modelContext),
					number -> number.intValue() - shift >= 0 ? (GenericModel) subContexts.get(number.intValue() - shift) : null,
					genericModel -> subContexts.indexOf(genericModel) + shift);
		});
	}

	private void bindMapElement(String name, Function<M, ObservableValue<String>> applyOnModel, Function<Model, Map<String, String>> getMap) {
		addPrefixBinding(modelContext -> {
			Map<String, String> map = getMap.apply(modelContext);
			ChangeListener<String> listener = (o, old, newValue) -> map.put(name, newValue);
			ObservableValue<String> observable = applyOnModel.apply((M) modelContext);
			observable.addListener(listener);
			map.put(name, observable.getValue());
		});
	}

	public void addStyle(String propertyName, String value) {
		addPrefixBinding(model -> model.getObservableStyles(this).put(propertyName, value));
	}

	public void bindOptionalStyle(String propertyName, Function<M, ObservableValue<Boolean>> applyOnModel, String propertyValue) {
		bindOptionalStyle(propertyName, applyOnModel, propertyValue, "");
	}

	public void bindOptionalStyle(String propertyName, Function<M, ObservableValue<Boolean>> applyOnModel, String propertyValue, String propertyValueFalse) {
		bindStyle(propertyName, model -> {
			ObservableValue<Boolean> optional = applyOnModel.apply(model);
			return Bindings.createStringBinding(() -> optional.getValue() ? propertyValue : propertyValueFalse, optional);
		});
	}

	public void bindStyle(String propertyName, Function<M, ObservableValue<String>> applyOnModel) {
		bindMapElement(propertyName, applyOnModel, model -> model.getObservableStyles(this));
	}

	public void setStyleClasses(Set<String> styleClasses) {
		addPrefixBinding(model -> {
			ObservableSet<String> observableStyleClasses = model.getObservableStyleClasses(this);
			observableStyleClasses.removeIf(styleClass -> !styleClasses.contains(styleClass));
			observableStyleClasses.addAll(styleClasses);
		});
	}

	public void addStyleClasses(Set<String> styleClasses) {
		addPrefixBinding(model -> model.getObservableStyleClasses(this).addAll(styleClasses));
	}

	public void bindStyleClasses(ObservableSet<String> styleClasses) {
		addPrefixBinding(model -> Bindings.bindContent(model.getObservableStyleClasses(this), styleClasses));
	}

	public void bindStylesMap(ObservableMap<String, String> styles) {
		addPrefixBinding(model -> Bindings.bindContent(model.getObservableStyles(this), styles));
	}

	public void setStylesMap(Map<String, String> styles) {
		addPrefixBinding(model -> model.getObservableStyles(this).putAll(styles));
	}

	public void addStyleClass(String styleClass) {
		addPrefixBinding(model -> model.getObservableStyleClasses(this).add(styleClass));
	}

	public void bindOptionalAttribute(String attributeName, Function<M, ObservableValue<Boolean>> applyOnModel, String attributeValue) {
		bindOptionalAttribute(attributeName, applyOnModel, attributeValue, null);
	}

	public void bindOptionalAttribute(String attributeName, Function<M, ObservableValue<Boolean>> applyOnModel, String attributeValue,
			String attributeValueFalse) {
		bindAttribute(attributeName, model -> {
			ObservableValue<Boolean> optional = applyOnModel.apply(model);
			return Bindings.createStringBinding(() -> optional.getValue() ? attributeValue : attributeValueFalse, optional);
		});
	}

	public void bindAttribute(String attributeName, Function<M, ObservableValue<String>> applyOnModel) {
		bindMapElement(attributeName, applyOnModel, model -> model.getObservableAttributes(this));
	}

	public void bindOperation(TriFunction<Generic[], Serializable, Generic, Generic> operation) {
		addPrefixBinding(modelContext -> ((InputGenericModel) modelContext).getInputAction().setValue(operation));
	}

	public void bindTextBidirectional(Function<M, Property<String>> applyOnModel) {
		addBidirectionalBinding(HtmlDomNode::getTextProperty, applyOnModel);
	}

	public void setText(String value) {
		addPrefixBinding(model -> model.getTextProperty(this).setValue(value));
	}

	public void bindText(Function<M, ObservableValue<String>> applyOnModel) {
		addPrefixBinding(modelContext -> modelContext.getTextProperty(this).bind(applyOnModel.apply((M) modelContext)));
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

		private final ChangeListener<String> textListener = (o, old,
				newValue) -> sendMessage(new JsonObject().put(MSG_TYPE, UPDATE_TEXT).put(ID, getId()).put(TEXT_CONTENT, newValue != null ? newValue : ""));

		private final MapChangeListener<String, String> stylesListener = change -> {
			if (!change.wasAdded() || change.getValueAdded() == null || change.getValueAdded().equals("")) {
				// System.out.println("Remove : " + change.getKey() + " " + change.getValueRemoved());
				sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_STYLE).put(ID, getId()).put(STYLE_PROPERTY, change.getKey()));
			} else if (change.wasAdded()) {
				// System.out.println("Add : " + change.getKey() + " " + change.getValueAdded());
				sendMessage(new JsonObject().put(MSG_TYPE, ADD_STYLE).put(ID, getId()).put(STYLE_PROPERTY, change.getKey()).put(STYLE_VALUE,
						change.getValueAdded()));
			}
		};

		private final MapChangeListener<String, String> attributesListener = change -> {
			if (!change.wasAdded() || change.getValueAdded() == null || change.getValueAdded().equals("")) {
				sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_ATTRIBUTE).put(ID, getId()).put(ATTRIBUTE_NAME, change.getKey()));
			} else if (change.wasAdded()) {
				sendMessage(new JsonObject().put(MSG_TYPE, ADD_ATTRIBUTE).put(ID, getId()).put(ATTRIBUTE_NAME, change.getKey()).put(ATTRIBUTE_VALUE,
						change.getValueAdded()));
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

		public void fillJson(JsonObject jsonObj) {
		}

		public void sendRemove() {
			sendMessage(new JsonObject().put(MSG_TYPE, REMOVE).put(ID, id));
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
			System.out.println(
					new JsonObject().put(MSG_TYPE, UPDATE_SELECTION).put(ID, getId()).put(SELECTED_INDEX, newValue != null ? newValue : 0).encodePrettily());
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
				System.out.println("Selected index : " + getSelectionIndex().getValue());
			}
		}

	}

	public class InputTextHtmlDomNode extends HtmlDomNode {
		public InputTextHtmlDomNode(String parentId) {
			super(parentId);
		}

		private final ObjectProperty<Consumer<Object>> enterProperty = new SimpleObjectProperty<>();

		@Override
		public void fillJson(JsonObject jsonObj) {
			super.fillJson(jsonObj);
			jsonObj.put("type", "text");
		}

		@Override
		public void handleMessage(JsonObject json) {
			if (ADD.equals(json.getString(MSG_TYPE)))
				getEnterProperty().get().accept(new Object());
			if (UPDATE.equals(json.getString(MSG_TYPE)))
				getTextProperty().setValue(json.getString(TEXT_CONTENT));
		}

		public ObjectProperty<Consumer<Object>> getEnterProperty() {
			return enterProperty;
		}

	}

	public class InputCheckHtmlDomNode extends HtmlDomNode {
		private static final String CHECKED = "checked";
		private Property<Boolean> checked = new SimpleBooleanProperty(false);
		private final String type;

		public InputCheckHtmlDomNode(String parentId, String type) {
			super(parentId);
			this.type = type;
		}

		public Property<Boolean> getChecked() {
			return checked;
		}

		@Override
		public void fillJson(JsonObject jsonObj) {
			super.fillJson(jsonObj);
			jsonObj.put("type", type);
			jsonObj.put(CHECKED, checked.getValue());
		}

		@Override
		public void handleMessage(JsonObject json) {
			if ("checkbox".equals(json.getString(ELT_TYPE)))
				getChecked().setValue(json.getBoolean(CHECKED));
		}
	}

}
