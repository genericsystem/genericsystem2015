package org.genericsystem.reactor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.StringExtractor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;
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

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 */
public abstract class Tag<M extends Model> {

	private static int count = 0;
	private static final Logger log = LoggerFactory.getLogger(Tag.class);
	private final String tag;
	private BiConsumer<Tag<?>, ViewContext<?>> metaBinding;
	private final List<BiConsumer<Model, HtmlDomNode>> preFixedBindings = new ArrayList<>();
	private final List<BiConsumer<Model, HtmlDomNode>> postFixedBindings = new ArrayList<>();
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

	protected List<BiConsumer<Model, HtmlDomNode>> getPreFixedBindings() {
		return preFixedBindings;
	}

	protected List<BiConsumer<Model, HtmlDomNode>> getPostFixedBindings() {
		return postFixedBindings;
	}

	protected BiConsumer<Tag<?>, ViewContext<?>> getMetaBinding() {
		return metaBinding;
	}

	protected void setMetaBinding(BiConsumer<Tag<?>, ViewContext<?>> metaBinding) {
		if (this.metaBinding != null)
			throw new IllegalStateException("MetaBinding already defined");
		this.metaBinding = metaBinding;
	}

	public ServerWebSocket getWebSocket() {
		return getParent().getWebSocket();
	}

	@FunctionalInterface
	public interface ModelPropertyConstructor<T> {
		TagProperty<T> build();
	}

	protected <W, NODE extends HtmlDomNode> void addBidirectionalBinding(Function<NODE, Property<W>> applyOnNode, Function<M, Property<W>> applyOnModel) {
		preFixedBindings.add((modelContext, node) -> applyOnNode.apply((NODE) node).bindBidirectional(applyOnModel.apply((M) modelContext)));
	}

	public void addPrefixBinding(Consumer<M> consumer) {
		preFixedBindings.add((modelContext, node) -> consumer.accept((M) modelContext));
	}

	public void addPostfixBinding(Consumer<M> consumer) {
		postFixedBindings.add((modelContext, node) -> consumer.accept((M) modelContext));
	}

	protected <NODE extends HtmlDomNode> void addActionBinding(Function<NODE, Property<Consumer<Object>>> applyOnNode, Consumer<M> applyOnModel) {
		preFixedBindings.add((modelContext, node) -> applyOnNode.apply((NODE) node).setValue(o -> applyOnModel.accept((M) modelContext)));
	}

	public <NODE extends HtmlDomNode> void bindOptionalStyleClass(String styleClass, TagProperty<Boolean> property) {
		addPrefixBinding(modelContext -> {
			ObservableValue<Boolean> optional = property.getObservable(((GenericModelInterface) modelContext).getGeneric());
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

	public <NODE extends HtmlDomNode> void bindOptionalStyleClass(String styleClass, ModelPropertyConstructor<Boolean> constructor, Function<M, ObservableValue<Boolean>> applyOnModel) {
		TagProperty<Boolean> property = storeProperty(constructor, applyOnModel);
		bindOptionalStyleClass(styleClass, property);
	}

	public <MODEL extends Model> void forEach(Function<MODEL, ObservableList<M>> applyOnModel) {
		setMetaBinding((childElement, viewContext) -> {
			MODEL model = viewContext.getModelContext();
			ObservableList<M> models = applyOnModel.apply(model);
			model.setSubContexts(childElement, new TransformationObservableList<M, MODEL>(models, (index, subModel) -> {
				subModel.parent = model;
				viewContext.createViewContextChild(index, subModel, childElement);
				return (MODEL) subModel;
			}, Model::destroy));
		});
	}

	protected void setSubModels(Model model, Tag<?> child, ObservableList<GenericModel> subModels) {
		model.setSubContexts(child, subModels);
	}

	@FunctionalInterface
	public interface ModelConstructor<M extends Model> {
		M build(Generic[] generics, StringExtractor stringExtractor);
	}

	public void addSelectionIndex(int value) {
		addPrefixBinding(modelContext -> modelContext.getSelectionIndex(this).setValue(value));
	}

	private void bindMapElement(String name, TagProperty<String> property, Function<Model, Map<String, String>> getMap) {
		addPrefixBinding(model -> {
			Map<String, String> map = getMap.apply(model);
			ChangeListener<String> listener = (o, old, newValue) -> map.put(name, newValue);
			ObservableValue<String> observable = property.getObservable(((GenericModelInterface) model).getGeneric());
			observable.addListener(listener);
			map.put(name, observable.getValue());
		});
	}

	private <T extends Serializable> void bindBiDirectionalMapElement(TagProperty<T> property, String name, Function<Model, ObservableMap<String, String>> getMap) {
		bindBiDirectionalMapElement(property, name, getMap, ApiStatics.STRING_CONVERTERS.get(String.class));
	}

	private <T extends Serializable> void bindBiDirectionalMapElement(TagProperty<T> property, String name, Function<Model, ObservableMap<String, String>> getMap, StringConverter<T> stringConverter) {
		bindBiDirectionalMapElement(property, name, getMap, model -> stringConverter);
	}

	private <T extends Serializable> void bindBiDirectionalMapElement(TagProperty<T> property, String name, Function<Model, ObservableMap<String, String>> getMap, Function<M, StringConverter<T>> getStringConverter) {
		addPrefixBinding(modelContext -> {
			ObservableMap<String, String> map = getMap.apply(modelContext);
			StringConverter<T> stringConverter = getStringConverter.apply(modelContext);
			ChangeListener<T> listener = (o, old, newValue) -> map.put(name, stringConverter.toString(newValue));
			Property<T> observable = property.getProperty(((GenericModelInterface) modelContext).getGeneric());
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

	public interface GenericModelInterface {
		public Generic getGeneric();
	}

	public <T extends Serializable> void bindActionToValueChangeListener(TagProperty<T> property, BiConsumer<M, T> listener) {
		addPrefixBinding(modelContext -> {
			property.getObservable(((GenericModelInterface) modelContext).getGeneric()).addListener((o, old, nva) -> listener.accept(modelContext, nva));
		});
	}

	// TODO: Move everything related to ModelProperty to GenericModel and GSTag since they use Generics.
	// No need to store in Model if Generics already stored in ModelProperty…
	public <T> TagProperty<T> createNewProperty(ModelPropertyConstructor<T> constructor) {
		TagProperty<T> modelProperty = constructor.build();
		addPrefixBinding(modelContext -> {
			modelProperty.setObservable(((GenericModelInterface) modelContext).getGeneric(), new SimpleObjectProperty<>());
			// modelContext.storeProperty(this, modelProperty);
		});
		return modelProperty;
	}

	public <T> void initProperty(TagProperty<T> property, T initialValue) {
		initProperty(property, model -> initialValue);
	}

	public <T> void initProperty(TagProperty<T> property, Function<M, T> getInitialValue) {
		addPrefixBinding(modelContext -> property.setValue(((GenericModelInterface) modelContext).getGeneric(), getInitialValue.apply(modelContext)));
	}

	public <T> TagProperty<T> storeProperty(ModelPropertyConstructor<T> constructor, Function<M, ObservableValue<T>> applyOnModel) {
		TagProperty<T> modelObservable = constructor.build();
		addPrefixBinding(modelContext -> {
			modelObservable.setObservable(((GenericModelInterface) modelContext).getGeneric(), applyOnModel.apply(modelContext));
			// modelContext.storeProperty(this, modelObservable);
		});
		return modelObservable;
	}

	public void addStyle(String stylePropertyName, String value) {
		addPrefixBinding(model -> model.getObservableStyles(this).put(stylePropertyName, value));
	}

	public void bindStyle(String style, TagProperty<String> property) {
		bindMapElement(style, property, model -> model.getObservableStyles(this));
	}

	public void bindStyle(String style, ModelPropertyConstructor<String> constructor, Function<M, ObservableValue<String>> applyOnModel) {
		TagProperty<String> property = storeProperty(constructor, applyOnModel);
		bindMapElement(style, property, model -> model.getObservableStyles(this));
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

	public void bindAttribute(String attributeName, TagProperty<String> property) {
		bindMapElement(attributeName, property, model -> model.getObservableAttributes(this));
	}

	public void bindAttribute(String attributeName, ModelPropertyConstructor<String> constructor, Function<M, ObservableValue<String>> applyOnModel) {
		TagProperty<String> property = storeProperty(constructor, applyOnModel);
		bindMapElement(attributeName, property, model -> model.getObservableAttributes(this));
	}

	public <T extends Serializable> void bindBiDirectionalAttribute(TagProperty<T> property, String attributeName) {
		bindBiDirectionalMapElement(property, attributeName, model -> model.getObservableAttributes(this));
	}

	public <T extends Serializable> void bindBiDirectionalAttribute(TagProperty<T> property, String attributeName, StringConverter<T> stringConverter) {
		bindBiDirectionalMapElement(property, attributeName, model -> model.getObservableAttributes(this), stringConverter);
	}

	public <T extends Serializable> void bindBiDirectionalAttribute(TagProperty<T> property, String attributeName, Function<M, StringConverter<T>> getStringConverter) {
		bindBiDirectionalMapElement(property, attributeName, model -> model.getObservableAttributes(this), getStringConverter);
	}

	public void bindOptionalBiDirectionalAttribute(TagProperty<Boolean> property, String attributeName, String attributeValue) {
		bindOptionalBiDirectionalAttribute(property, attributeName, attributeValue, null);
	}

	public void bindOptionalBiDirectionalAttribute(TagProperty<Boolean> property, String attributeName, String attributeValue, String attributeValueFalse) {
		bindBiDirectionalMapElement(property, attributeName, model -> model.getObservableAttributes(this), new StringConverter<Boolean>() {

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

		private final ChangeListener<String> textListener = (o, old, newValue) -> sendMessage(new JsonObject().put(MSG_TYPE, UPDATE_TEXT).put(ID, getId()).put(TEXT_CONTENT, newValue != null ? newValue : ""));

		private final MapChangeListener<String, String> stylesListener = change -> {
			if (!change.wasAdded() || change.getValueAdded() == null || change.getValueAdded().equals(""))
				sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_STYLE).put(ID, getId()).put(STYLE_PROPERTY, change.getKey()));
			else if (change.wasAdded())
				sendMessage(new JsonObject().put(MSG_TYPE, ADD_STYLE).put(ID, getId()).put(STYLE_PROPERTY, change.getKey()).put(STYLE_VALUE, change.getValueAdded()));
		};

		private final MapChangeListener<String, String> attributesListener = change -> {
			if (!change.wasAdded() || change.getValueAdded() == null || change.getValueAdded().equals(""))
				sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_ATTRIBUTE).put(ID, getId()).put(ATTRIBUTE_NAME, change.getKey()));
			else if (change.wasAdded())
				sendMessage(new JsonObject().put(MSG_TYPE, ADD_ATTRIBUTE).put(ID, getId()).put(ATTRIBUTE_NAME, change.getKey()).put(ATTRIBUTE_VALUE, change.getValueAdded()));
		};

		private final SetChangeListener<String> styleClassesListener = change -> {
			if (change.wasAdded())
				sendMessage(new JsonObject().put(MSG_TYPE, ADD_STYLECLASS).put(ID, getId()).put(STYLECLASS, change.getElementAdded()));
			else
				sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_STYLECLASS).put(ID, getId()).put(STYLECLASS, change.getElementRemoved()));
		};

		public ObservableMap<String, String> getStyles() {
			return styles;
		}

		public ObservableMap<String, String> getAttributes() {
			return attributes;
		}

		public HtmlDomNode(String parentId) {
			assert parentId != null;
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
			jsonObj.put("count", count++);
			// if (jsonObj.getString(MSG_TYPE).equals(ADD) || jsonObj.getString(MSG_TYPE).equals(REMOVE))
			// System.out.println(jsonObj.encodePrettily());
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
		private final Property<Consumer<Object>> enterProperty = new SimpleObjectProperty<>();

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
				getEnterProperty().getValue().accept(new Object());
			if (UPDATE.equals(json.getString(MSG_TYPE))) {
				getTextProperty().setValue(json.getString(TEXT_CONTENT));
				getAttributes().put(ReactorStatics.VALUE, json.getString(TEXT_CONTENT));
			}
		}

		public Property<Consumer<Object>> getEnterProperty() {
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
