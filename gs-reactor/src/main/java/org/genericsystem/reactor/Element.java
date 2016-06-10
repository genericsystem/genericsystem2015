package org.genericsystem.reactor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Function;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.composite.CompositeModel;
import org.genericsystem.reactor.composite.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeModel.StringExtractor;

import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;
import javafx.beans.binding.ListBinding;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 */
public abstract class Element<M extends Model> {

	private final String tag;
	private List<Boot<HtmlDomNode>> boots = new ArrayList<>();
	public MetaBinding<?> metaBinding;
	public final List<Binding<?, ?>> bindings = new ArrayList<>();
	private final Element<?> parent;
	private final List<Element<?>> children = new ArrayList<>();

	@Override
	public String toString() {
		return getClass().getSimpleName();
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

	protected <VALUE> void addBoot(Function<HtmlDomNode, Property<VALUE>> applyOnNode, VALUE value) {
		this.boots.add(Boot.setProperty(applyOnNode, value));
	}

	protected <VALUE> void addSetBoot(Function<HtmlDomNode, Set<VALUE>> applyOnNode, VALUE value) {
		this.boots.add(Boot.addProperty(applyOnNode, value));
	}

	protected List<Boot<HtmlDomNode>> getBootList() {
		return boots;
	}

	protected <W, NODE extends HtmlDomNode> void addBidirectionalBinding(Function<NODE, Property<W>> applyOnNode, Function<M, Property<W>> applyOnModel) {
		bindings.add(Binding.bindBiDirectionalProperty(applyOnModel, applyOnNode));

	}

	protected <T, NODE extends HtmlDomNode> void addBinding(Function<NODE, Property<T>> applyOnNode, Function<M, ObservableValue<T>> applyOnModel) {
		bindings.add(Binding.bindProperty(applyOnModel, applyOnNode));

	}

	protected <T, NODE extends HtmlDomNode> void addActionBinding(Function<NODE, Property<T>> applyOnNode, Consumer<M> applyOnModel) {
		bindings.add(Binding.bindAction(applyOnModel, applyOnNode));
	}

	protected <T, NODE extends HtmlDomNode> void addReversedBinding(Function<NODE, ObservableValue<T>> applyOnNode, Function<M, Property<T>> applyOnModel) {
		bindings.add(Binding.bindReversedProperty(applyOnModel, applyOnNode));

	}

	protected <T, NODE extends HtmlDomNode> void addSetBinding(Function<NODE, Set<T>> applyOnNode, Function<M, ObservableValue<Boolean>> applyOnModel,
			T styleClass) {
		bindings.add(Binding.bindSet(applyOnModel, styleClass, applyOnNode));
	}

	public <T extends Model> void forEach(Function<T, ObservableList<M>> applyOnModel) {
		metaBinding = MetaBinding.forEach(applyOnModel);
	}

	public void forEach(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, ModelConstructor<CompositeModel> constructor) {
		forEach(model -> ((CompositeModel) model).getObservableList(stringExtractor, observableListExtractor, constructor));
	}

	public <T extends Model> void select(Function<T, ObservableValue<M>> applyOnModel) {
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

	public void select(StringExtractor stringExtractor, Function<Generic[], Generic> genericSupplier, ModelConstructor<CompositeModel> constructor) {
		forEach(model -> ((CompositeModel) model).getObservableList(stringExtractor, gs -> (genericSupplier.apply(gs) != null
				? FXCollections.singletonObservableList(genericSupplier.apply(gs)) : FXCollections.emptyObservableList()), constructor));
	}

	public void select(StringExtractor stringExtractor, Class<?> genericClass, ModelConstructor<CompositeModel> constructor) {
		forEach(model -> ((CompositeModel) model).getObservableList(stringExtractor,
				gs -> FXCollections.singletonObservableList(gs[0].getRoot().find(genericClass)), constructor));
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

	public void addStyleClass(String styleClass) {
		addSetBoot(HtmlDomNode::getStyleClasses, styleClass);
	}

	public void addStyle(String propertyName, String value) {
		addBoot(domNode -> domNode.getStyle(propertyName), value);
	}

	public void setText(String text) {
		addBoot(HtmlDomNode::getText, text);
	}

	public void bindTextBidirectional(Function<M, Property<String>> applyOnModel) {
		addBidirectionalBinding(HtmlDomNode::getText, applyOnModel);
	}

	public void bindText(Function<M, ObservableValue<String>> applyOnModel) {
		addBinding(HtmlDomNode::getText, applyOnModel);
	}

	public void bindStyle(String propertyName, Function<M, ObservableValue<String>> applyOnModel) {
		addBinding(domNode -> domNode.getStyle(propertyName), applyOnModel);
	}

	public void bindStyle(String propertyName, String initialValue) {
		bindStyle(propertyName, model -> ((CompositeModel) model).getObservableStyle(this, propertyName, initialValue));
	}

	public void bindStyle(String propertyName) {
		bindStyle(propertyName, model -> ((CompositeModel) model).getStyleProperty(this, propertyName));
	}

	public void bindOptionalStyleClass(Function<M, ObservableValue<Boolean>> function, String text) {
		addSetBinding(HtmlDomNode::getStyleClasses, function, text);
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

		private final Set<String> styleClasses = new HashSet<String>() {

			private static final long serialVersionUID = -7679372997269319684L;

			@Override
			public boolean add(String styleClass) {
				boolean result = super.add(styleClass);
				if (result) {
					System.out.println(new JsonObject().put(MSG_TYPE, ADD_STYLECLASS).put(ID, id).put(STYLECLASS, styleClass).encodePrettily());
					sendMessage(new JsonObject().put(MSG_TYPE, ADD_STYLECLASS).put(ID, id).put(STYLECLASS, styleClass));
				}
				return result;
			};

			@Override
			public boolean remove(Object styleClass) {
				boolean result = super.remove(styleClass);
				if (result) {
					System.out.println(new JsonObject().put(MSG_TYPE, REMOVE_STYLECLASS).put(ID, id).put(STYLECLASS, styleClass).encodePrettily());
					sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_STYLECLASS).put(ID, id).put(STYLECLASS, styleClass));
				}
				return result;
			};
		};

		private final Map<String, String> styles = new HashMap<String, String>() {

			private static final long serialVersionUID = 3900526227565046414L;

			@Override
			public String put(String propertyName, String value) {
				String result = super.put(propertyName, value);
				if (!Objects.equals(value, result)) {
					JsonObject mapJS = new JsonObject();
					mapJS.put(propertyName, value);
					if (value != null && !value.isEmpty())
						sendMessage(new JsonObject().put(MSG_TYPE, ADD_STYLE).put(ID, id).put(STYLE_PROPERTY, propertyName).put(STYLE_VALUE, value));
					else
						sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_STYLE).put(ID, id).put(STYLE_PROPERTY, propertyName));
				}
				return result;
			}
		};

		public HtmlDomNode(String parentId) {
			this.parentId = parentId;
			this.id = String.format("%010d", Integer.parseInt(this.hashCode() + "")).substring(0, 10);
			text.addListener((c, o, n) -> {
				System.out.println(new JsonObject().put(MSG_TYPE, UPDATE_TEXT).put(ID, id).put(TEXT_CONTENT, n != null ? n : "").encodePrettily());
				sendMessage(new JsonObject().put(MSG_TYPE, UPDATE_TEXT).put(ID, id).put(TEXT_CONTENT, n != null ? n : ""));
			});
		}

		public void sendAdd(int index) {
			JsonObject jsonObj = new JsonObject().put(MSG_TYPE, ADD);
			fillJson(jsonObj);
			jsonObj.put(PREV_ID, index);
			System.out.println(jsonObj.encodePrettily());
			sendMessage(jsonObj);
		}

		public void sendRemove() {
			JsonObject jsonObj = new JsonObject().put(MSG_TYPE, REMOVE);
			jsonObj.put(ID, id);
			sendMessage(jsonObj);
		}

		public void sendMessage(JsonObject jsonObj) {
			getWebSocket().writeFinalTextFrame(jsonObj.encode());
		}

		void fillJson(JsonObject jsonObj) {
			jsonObj.put(PARENT_ID, parentId);
			jsonObj.put(ID, id);
			jsonObj.put(TAG_HTML, getTag());
		}

		public Set<String> getStyleClasses() {
			return styleClasses;
		}

		public Property<String> getStyle(String propertyName) {
			Property<String> property = new SimpleStringProperty(styles.get(propertyName));
			property.addListener((c, o, n) -> styles.put(propertyName, n));
			return property;
		}

		public StringProperty getText() {
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

		private final ObjectProperty<EventHandler<ActionEvent>> actionProperty = new SimpleObjectProperty<>();

		public ObjectProperty<EventHandler<ActionEvent>> getActionProperty() {
			return actionProperty;
		}

		@Override
		public void handleMessage(JsonObject json) {
			if (ADD.equals(json.getString(MSG_TYPE)))
				getActionProperty().get().handle(new ActionEvent());
			super.handleMessage(json);
		}

	}

	public class InputTextHtmlDomNode extends HtmlDomNode {
		public InputTextHtmlDomNode(String parentId) {
			super(parentId);
		}

		private final ObjectProperty<EventHandler<ActionEvent>> enterProperty = new SimpleObjectProperty<>();

		@Override
		public void fillJson(JsonObject jsonObj) {
			super.fillJson(jsonObj);
			jsonObj.put("type", "text");
		}

		@Override
		public void handleMessage(JsonObject json) {
			if (ADD.equals(json.getString(MSG_TYPE)))
				getEnterProperty().get().handle(new ActionEvent());
			if (UPDATE.equals(json.getString(MSG_TYPE)))
				getText().setValue(json.getString(TEXT_CONTENT));
			super.handleMessage(json);
		}

		public ObjectProperty<EventHandler<ActionEvent>> getEnterProperty() {
			return enterProperty;
		}

	}

	public class CheckBoxHtmlDomNode extends HtmlDomNode {
		public CheckBoxHtmlDomNode(String parentId) {
			super(parentId);
		}

		private static final String CHECKED = "checked";

		private Property<Boolean> checked = new SimpleBooleanProperty(false);

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
			super.handleMessage(json);
		}
	}

}
