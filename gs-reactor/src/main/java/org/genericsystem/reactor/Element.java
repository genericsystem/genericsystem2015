package org.genericsystem.reactor;

import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.WeakListener;
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

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.composite.CompositeElement;
import org.genericsystem.reactor.model.CompositeModel;
import org.genericsystem.reactor.model.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.model.CompositeModel.StringExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.SelectorModel;

import com.sun.javafx.binding.Logging;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 */
public abstract class Element<M extends Model> {

	private final String tag;
	public BiConsumer<Element<M>, ViewContext<?>> metaBinding;
	public final List<BiConsumer<ModelContext, HtmlDomNode>> preFixedBindings = new ArrayList<>();
	public final List<BiConsumer<ModelContext, HtmlDomNode>> postFixedBindings = new ArrayList<>();
	private final Element<?> parent;
	private final List<Element<?>> children = new ArrayList<>();

	@Override
	public String toString() {
		return tag + " " + getClass().getName();
	}

	protected Element(Element<?> parent, String tag) {
		this.tag = tag;
		this.parent = parent;
		if (parent != null)
			parent.getChildren().add(this);
	}

	public String getTag() {
		return tag;
	}

	protected <W, NODE extends HtmlDomNode> void addBidirectionalBinding(Function<NODE, Property<W>> applyOnNode, Function<M, Property<W>> applyOnModel) {
		preFixedBindings.add((modelContext, node) -> applyOnNode.apply((NODE) node).bindBidirectional(applyOnModel.apply(modelContext.getModel())));
	}

	@Deprecated
	protected <T, NODE extends HtmlDomNode> void addBinding(Function<NODE, Property<T>> applyOnNode, Function<M, ObservableValue<T>> applyOnModel) {
		preFixedBindings.add((modelContext, node) -> applyOnNode.apply((NODE) node).bind(applyOnModel.apply(modelContext.getModel())));
	}

	protected void addPrefixBinding(Consumer<ModelContext> consumer) {
		preFixedBindings.add((modelContext, node) -> consumer.accept(modelContext));
	}

	protected void addPostFixBinding(Consumer<ModelContext> consumer) {
		postFixedBindings.add((modelContext, node) -> consumer.accept(modelContext));
	}

	@Deprecated
	protected <NODE extends HtmlDomNode> void addMapBinding(Function<NODE, Map<String, String>> applyOnNode, Function<M, ObservableMap<String, String>> applyOnModel) {
		preFixedBindings.add((modelContext, node) -> Bindings.bindContent(applyOnNode.apply((NODE) node), applyOnModel.apply(modelContext.getModel())));
	}

	protected <NODE extends HtmlDomNode> void addActionBinding(Function<NODE, Property<Consumer<Object>>> applyOnNode, Consumer<M> applyOnModel) {
		preFixedBindings.add((modelContext, node) -> applyOnNode.apply((NODE) node).setValue(o -> applyOnModel.accept(modelContext.getModel())));
	}

	public <NODE extends HtmlDomNode> void bindOptionalStyleClass(Function<M, ObservableValue<Boolean>> applyOnModel, String styleClass) {
		addPrefixBinding(modelContext -> {
			ObservableValue<Boolean> optional = applyOnModel.apply(modelContext.getModel());
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

	@Deprecated
	protected <NODE extends HtmlDomNode> void addSetBinding(Function<NODE, Set<String>> applyOnNode, Function<M, ObservableSet<String>> applyOnModel) {
		preFixedBindings.add((modelContext, node) -> Bindings.bindContent(applyOnNode.apply((NODE) node), applyOnModel.apply(modelContext.getModel())));
	}

	public <T extends Model> void forEach(Function<T, ObservableList<M>> applyOnModel) {
		contextForEach(modelContext -> applyOnModel.apply(modelContext.getModel()));
	}

	protected void contextForEach(Function<ModelContext, ObservableList<M>> applyOnModelContext) {
		metaBinding = (childElement, viewContext) -> {
			ModelContext modelContext = viewContext.getModelContext();
			modelContext.getSubContextsMap().put(childElement,
					new TransformationObservableList<M, ModelContext>(applyOnModelContext.apply(modelContext), (index, model) -> modelContext.createChildContext(model, viewContext, index, childElement), ModelContext::destroy));
		};
	}

	public void forEach(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, ModelConstructor<CompositeModel> constructor) {
		contextForEach(modelContext -> modelContext.setObservableSubModels(this, stringExtractor, observableListExtractor, constructor));
	}

	public <T extends Model> void select(Function<T, ObservableValue<M>> applyOnModel) {
		contextForEach(modelContext -> {
			ObservableValue<M> observableValue = applyOnModel.apply(modelContext.getModel());
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

	public void select(StringExtractor stringExtractor, Function<Generic[], Generic> genericSupplier, ModelConstructor<CompositeModel> constructor) {
		contextForEach(modelContext -> modelContext.setObservableSubModels(this, stringExtractor, gs -> {
			Generic generic = genericSupplier.apply(gs);
			return generic != null ? FXCollections.singletonObservableList(generic) : FXCollections.emptyObservableList();
		}, constructor));
	}

	public void select(StringExtractor stringExtractor, Class<?> genericClass, ModelConstructor<CompositeModel> constructor) {
		contextForEach(modelContext -> modelContext.setObservableSubModels(this, stringExtractor, gs -> FXCollections.singletonObservableList(gs[0].getRoot().find(genericClass)), constructor));
		// forEach(stringExtractor, gs -> gs[0].getRoot().find(genericClass), constructor);
	}

	public ServerWebSocket getWebSocket() {
		return getParent().getWebSocket();
	}

	public void forEach_(ObservableListExtractor observableListExtractor) {
		forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, observableListExtractor, CompositeModel::new);
	}

	public void forEach(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor) {
		forEach(stringExtractor, observableListExtractor, CompositeModel::new);
	}

	public void select(StringExtractor stringExtractor, Function<Generic[], Generic> genericSupplier) {
		select(stringExtractor, genericSupplier, CompositeModel::new);
	}

	public void select(Function<Generic[], Generic> genericSupplier, ModelConstructor<CompositeModel> constructor) {
		select(StringExtractor.SIMPLE_CLASS_EXTRACTOR, genericSupplier, constructor);
	}

	public void select_(Function<Generic[], Generic> genericSupplier) {
		select(StringExtractor.SIMPLE_CLASS_EXTRACTOR, genericSupplier, CompositeModel::new);
	}

	public void select(StringExtractor stringExtractor, Class<?> genericClass) {
		select(stringExtractor, genericClass, CompositeModel::new);
	}

	public void select(Class<?> genericClass) {
		select(StringExtractor.SIMPLE_CLASS_EXTRACTOR, genericClass);
	}

	public void addSelectionIndex(int value) {
		addPrefixBinding(modelContext -> modelContext.getSelectionIndex(this).setValue(value));
	}

	public void bindSelectionIndex(Function<M, ObservableValue<Number>> applyOnModel) {
		addPrefixBinding(modelContext -> modelContext.getSelectionIndex(this).bind(applyOnModel.apply(modelContext.getModel())));
	}

	public void bindBidirectionalSelectionIndex(Function<M, Property<Number>> applyOnModel) {
		addPrefixBinding(modelContext -> modelContext.getSelectionIndex(this).bindBidirectional(applyOnModel.apply(modelContext.getModel())));
	}

	private <SUBMODEL extends CompositeModel> void bindBiDirectionalSelection(Element<SUBMODEL> subElement) {
		bindBiDirectionalSelection(subElement, SelectorModel::getSelection);
	}

	protected <SUBMODEL extends CompositeModel> void bindBiDirectionalSelection(Element<SUBMODEL> subElement, Function<SelectorModel, Property<CompositeModel>> applyOnModel) {
		addPostFixBinding(modelContext -> {
			ObservableList<SUBMODEL> observableList = modelContext.<M, SUBMODEL> getObservableSubModels(subElement);
			assert observableList != null;
			Property<Number> indexSelection = modelContext.getSelectionIndex(this);
			Property<CompositeModel> selection = applyOnModel.apply(modelContext.getModel());
			bindBidirectional(indexSelection, selection, number -> observableList.get(number.intValue()), generic -> observableList.indexOf(generic));

		});
	}

	public static <S, T> Object bindBidirectional(Property<S> property, Property<T> otherProperty, Function<S, T> to, Function<T, S> from) {
		final SBidirectionalBinding<S, T> binding = new SBidirectionalBinding<S, T>(property, otherProperty, to, from);
		property.setValue(from.apply(otherProperty.getValue()));
		property.addListener(binding);
		otherProperty.addListener(binding);
		return binding;
	}

	private static class SBidirectionalBinding<S, T> implements ChangeListener<Object>, WeakListener {

		private final WeakReference<Property<S>> stringPropertyRef;
		private final WeakReference<Property<T>> otherPropertyRef;
		private boolean updating;
		private final Function<S, T> fromS;
		private final Function<T, S> toS;

		// private BidirectionalBinding<Object> kkk;

		public SBidirectionalBinding(Property<S> stringProperty, Property<T> otherProperty, Function<S, T> fromS, Function<T, S> toS) {
			stringPropertyRef = new WeakReference<Property<S>>(stringProperty);
			otherPropertyRef = new WeakReference<Property<T>>(otherProperty);
			cachedHashCode = stringProperty.hashCode() * otherProperty.hashCode();
			this.fromS = fromS;
			this.toS = toS;
		}

		protected S toString(T value) {
			return toS.apply(value);
		};

		protected T fromString(S value) {
			return fromS.apply(value);
		}

		protected Object getProperty1() {
			return stringPropertyRef.get();
		}

		protected Object getProperty2() {
			return otherPropertyRef.get();
		}

		@Override
		public void changed(ObservableValue<? extends Object> observable, Object oldValue, Object newValue) {
			if (!updating) {
				final Property<S> property1 = stringPropertyRef.get();
				final Property<T> property2 = otherPropertyRef.get();
				if ((property1 == null) || (property2 == null)) {
					if (property1 != null) {
						property1.removeListener(this);
					}
					if (property2 != null) {
						property2.removeListener(this);
					}
				} else {
					try {
						updating = true;
						if (property1 == observable) {
							try {
								property2.setValue(fromString(property1.getValue()));
							} catch (Exception e) {
								Logging.getLogger().warning("Exception while parsing String in bidirectional binding", e);
								property2.setValue(null);
							}
						} else {
							try {
								property1.setValue(toString(property2.getValue()));
							} catch (Exception e) {
								Logging.getLogger().warning("Exception while converting Object to String in bidirectional binding", e);
								property1.setValue(null);
							}
						}
					} finally {
						updating = false;
					}
				}
			}
		}

		private final int cachedHashCode;

		@Override
		public int hashCode() {
			return cachedHashCode;
		}

		@Override
		public boolean wasGarbageCollected() {
			return (getProperty1() == null) || (getProperty2() == null);
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj) {
				return true;
			}

			final Object propertyA1 = getProperty1();
			final Object propertyA2 = getProperty2();
			if ((propertyA1 == null) || (propertyA2 == null)) {
				return false;
			}

			if (obj instanceof SBidirectionalBinding) {
				final SBidirectionalBinding otherBinding = (SBidirectionalBinding) obj;
				final Object propertyB1 = otherBinding.getProperty1();
				final Object propertyB2 = otherBinding.getProperty2();
				if ((propertyB1 == null) || (propertyB2 == null)) {
					return false;
				}

				if (propertyA1 == propertyB1 && propertyA2 == propertyB2) {
					return true;
				}
				if (propertyA1 == propertyB2 && propertyA2 == propertyB1) {
					return true;
				}
			}
			return false;
		}
	}

	public void addStyle(String propertyName, String value) {
		addPrefixBinding(model -> model.getObservableStyles(this).put(propertyName, value));
	}

	@Deprecated
	public void addStyle(String propertyName, Function<M, String> applyOnModel) {
		addPrefixBinding(modelContext -> modelContext.getObservableStyles(this).put(propertyName, applyOnModel.apply(modelContext.getModel())));
	}

	public void bindStyle(String propertyName, Function<M, ObservableValue<String>> applyOnModel) {
		addPrefixBinding(modelContext -> {
			Map<String, String> stylesMap = modelContext.getObservableStyles(this);
			ChangeListener<String> listener = (o, old, newValue) -> stylesMap.put(propertyName, newValue);
			ObservableValue<String> observableStyle = applyOnModel.apply(modelContext.getModel());
			observableStyle.addListener(listener);
			stylesMap.put(propertyName, observableStyle.getValue());
		});
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

	public void bindTextBidirectional(Function<M, Property<String>> applyOnModel) {
		addBidirectionalBinding(HtmlDomNode::getTextProperty, applyOnModel);
	}

	public void setText(String value) {
		addPrefixBinding(model -> model.getTextProperty(this).setValue(value));
	}

	public void bindText(Function<M, ObservableValue<String>> applyOnModel) {
		addPrefixBinding(modelContext -> modelContext.getTextProperty(this).bind(applyOnModel.apply(modelContext.getModel())));
	}

	protected void forEach(CompositeElement<?> parentCompositeElement) {
		forEach(g -> parentCompositeElement.getStringExtractor().apply(g), gs -> parentCompositeElement.getObservableListExtractor().apply(gs), (gs, extractor) -> parentCompositeElement.getModelConstructor().build(gs, extractor));
	}

	protected abstract HtmlDomNode createNode(String parentId);

	protected List<Element<?>> getChildren() {
		return children;
	}

	@SuppressWarnings("unchecked")
	public <COMPONENT extends Element<?>> COMPONENT getParent() {
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

	private static final String PARENT_ID = "parentId";
	public static final String ID = "nodeId";
	private static final String PREV_ID = "nextId";
	private static final String STYLE_PROPERTY = "styleProperty";
	private static final String STYLE_VALUE = "styleValue";
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

		private final ChangeListener<String> textListener = (o, old, newValue) -> sendMessage(new JsonObject().put(MSG_TYPE, UPDATE_TEXT).put(ID, getId()).put(TEXT_CONTENT, newValue != null ? newValue : ""));

		private final MapChangeListener<String, String> stylesListener = change -> {
			if (change.wasRemoved() && (!change.wasAdded() || change.getValueAdded() == null || change.getValueAdded().equals(""))) {
				// System.out.println("Remove : " + change.getKey() + " " + change.getValueRemoved());
				sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_STYLE).put(ID, getId()).put(STYLE_PROPERTY, change.getKey()));
			}
			if (change.wasAdded()) {
				// System.out.println("Add : " + change.getKey() + " " + change.getValueAdded());
				sendMessage(new JsonObject().put(MSG_TYPE, ADD_STYLE).put(ID, getId()).put(STYLE_PROPERTY, change.getKey()).put(STYLE_VALUE, change.getValueAdded()));
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

		public HtmlDomNode(String parentId) {
			this.parentId = parentId;
			this.id = String.format("%010d", Integer.parseInt(this.hashCode() + "")).substring(0, 10);
			text.addListener(new WeakChangeListener<>(textListener));
			styles.addListener(new WeakMapChangeListener<>(stylesListener));
			styleClasses.addListener(new WeakSetChangeListener<>(styleClassesListener));
		}

		public void sendAdd(int index) {
			JsonObject jsonObj = new JsonObject().put(MSG_TYPE, ADD);
			jsonObj.put(PARENT_ID, parentId);
			jsonObj.put(ID, id);
			jsonObj.put(TAG_HTML, getTag());
			jsonObj.put(PREV_ID, index);
			fillJson(jsonObj);
			// System.out.println(jsonObj.encodePrettily());
			sendMessage(jsonObj);
		}

		public void fillJson(JsonObject jsonObj) {
		}

		public void sendRemove() {
			JsonObject jsonObj = new JsonObject().put(MSG_TYPE, REMOVE);
			jsonObj.put(ID, id);
			sendMessage(jsonObj);
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
			System.out.println(new JsonObject().put(MSG_TYPE, UPDATE_SELECTION).put(ID, getId()).put(SELECTED_INDEX, newValue != null ? newValue : 0).encodePrettily());
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

	public class CheckBoxHtmlDomNode extends HtmlDomNode {
		private static final String CHECKED = "checked";
		private Property<Boolean> checked = new SimpleBooleanProperty(false);

		public CheckBoxHtmlDomNode(String parentId) {
			super(parentId);
		}

		public Property<Boolean> getChecked() {
			return checked;
		}

		@Override
		public void fillJson(JsonObject jsonObj) {
			super.fillJson(jsonObj);
			jsonObj.put("type", "checkbox");
			jsonObj.put(CHECKED, checked.getValue());
		}

		@Override
		public void handleMessage(JsonObject json) {
			if ("checkbox".equals(json.getString(ELT_TYPE)))
				getChecked().setValue(json.getBoolean(CHECKED));
		}
	}

}
