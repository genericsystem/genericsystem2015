package org.genericsystem.genericApp;

import java.util.Objects;
import java.util.function.Function;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Group;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableColumn.CellDataFeatures;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.util.Callback;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;
import org.genericsystem.distributed.cacheonclient.CocServer;
import org.genericsystem.kernel.Statics;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.utils.Transformation;

public abstract class AbstractGenericList {
	protected Transformation<GenericWrapper, Generic> genericWrapperList;
	protected ObservableList<Generic> dependenciesObservableList;
	protected Property<GenericWrapper> property = new SimpleObjectProperty<>();

	protected Property<GenericWrapper> getProperty() {
		return property;
	}

	public static class GenericWrapper extends AbstractGenericList {

		private Generic generic;
		private StringProperty stringProperty = new SimpleStringProperty();
		private ObservableValue<String> removeButtonTextProperty = Bindings.concat("Remove : ", stringProperty);

		public GenericWrapper(Generic generic) {
			this.generic = generic;
			stringProperty.set(Objects.toString(this.generic.getValue()));

			dependenciesObservableList = FXCollections.observableArrayList(generic.getInstances().toList());
			System.out.println(dependenciesObservableList.size());
			genericWrapperList = new Transformation<GenericWrapper, Generic>(dependenciesObservableList, t -> new GenericWrapper(t));
		}

		public void remove() {
			generic.remove();
		}

		public ObservableList<GenericWrapper> getGenericWrapperListInstances() {
			return genericWrapperList;
		}

		public ObservableValue<String> getObservable() {
			return stringProperty;
		}

		public ObservableValue<String> getRemoveButtonTextProperty() {
			return removeButtonTextProperty;
		}

		// public static void initialize() {
		// Callback<CellDataFeatures<GenericWrapper, String>, ObservableValue<String>> callback = features -> new SimpleObjectProperty<>(features.getValue().getObservable().getValue());
		// Callback<TableColumn<GenericWrapper, String>, TableCell<GenericWrapper, String>> callbackDelete = column -> new DeleteButtonCell<>(GenericWrapper::remove);
		//
		// Element<TableView> todoTableView2 = new Element<>(mainVBox, TableView.class);
		// todoTableView2.addSelectorMetaBinding(GenericList::getProperty);
		//
		// Element<GenericWrapper> todoTableItems2 = new Element<>(todoTableView2, GenericWrapper.class, TableView<GenericWrapper>::getItems);
		// todoTableItems2.addForEachMetaBinding(GenericWrapper::getGenericWrapperListInstances);
		// Function<TableView<GenericWrapper>, ObservableList<?>> getItems = TableView::getItems;
		// Function<TableView<?>, ObservableList<?>> getColumns = TableView::getColumns;
		//
		// Element<TableColumn> columnTodo = new Element<>(todoTableView2, TableColumn.class, getColumns);
		//
		// columnTodo.addBoot(TableColumn<GenericWrapper, String>::prefWidthProperty, 100);
		// columnTodo.addBoot(TableColumn<GenericWrapper, String>::textProperty, "instance");
		// columnTodo.addBoot(TableColumn<GenericWrapper, String>::cellValueFactoryProperty, callback);
		//
		// Element<TableColumn> columnDeleteTodo = new Element<>(todoTableView2, TableColumn.class, getColumns);
		//
		// columnDeleteTodo.addBoot(TableColumn<GenericWrapper, String>::prefWidthProperty, 150);
		// columnDeleteTodo.addBoot(TableColumn<GenericWrapper, String>::textProperty, "Delete todo");
		// columnDeleteTodo.addBoot(TableColumn<GenericWrapper, String>::cellValueFactoryProperty, callback);
		// columnDeleteTodo.addBoot(TableColumn<GenericWrapper, String>::cellFactoryProperty, callbackDelete);
		// }

	}

	static Element<VBox> mainVBox;

	public static class GenericList extends AbstractGenericList {

		private final String path = System.getenv("HOME") + "/test/ObservableListChain";

		private CocServer server;
		private CocClientEngine engine;

		private Property<String> name = new SimpleStringProperty();
		private ObservableList<Column> columns = FXCollections.observableArrayList(new Column(), new DeleteColumn());
		private ObservableValue<Number> height = new SimpleDoubleProperty(400);

		private Property<String> columnTitle = new SimpleStringProperty();

		public GenericList() throws InterruptedException {
			server = new CocServer(new GSDeploymentOptions(Statics.ENGINE_VALUE, Statics.DEFAULT_PORT, path));
			server.start();

			engine = new CocClientEngine(Statics.ENGINE_VALUE, null, Statics.DEFAULT_PORT);

			dependenciesObservableList = engine.getCurrentCache().getObservableDependencies(engine);

			genericWrapperList = new Transformation<GenericWrapper, Generic>(dependenciesObservableList, generic -> new GenericWrapper(generic));
		}

		public Property getName() {
			return name;
		}

		public ObservableList<GenericWrapper> getGenerics() {
			return genericWrapperList;
		}

		public void flush() {
			engine.getCurrentCache().flush();
		}

		public void clear() {
			engine.getCurrentCache().clear();

		}

		public void mount() {
			engine.getCurrentCache().mount();
		}

		public void unmount() {
			engine.getCurrentCache().unmount();
		}

		public void create() {
			engine.addInstance(name.getValue());
		}

		public class Column extends TableColumn<GenericWrapper, String> {
			public Column() {
				super("Todos");
				setMinWidth(130);
				setCellValueFactory(features -> new ReadOnlyObjectWrapper<String>(features.getValue().getObservable().getValue()));
			}
		}

		public class DeleteColumn extends Column {
			public DeleteColumn() {
				setText("Delete");
				setMinWidth(130);
				// setCellFactory(column -> new DeleteButtonCell<>(GenericWrapper::remove));
			}
		}

		public ObservableList<Column> getColumns() {
			return columns;
		}

		public Node initTable(Group scene) {
			Element<Group> sceneElt = new Element<>(Group.class);
			Element<VBox> mainVBox = new Element<>(sceneElt, VBox.class, Group::getChildren);

			mainVBox.addBoot(VBox::prefHeightProperty, 600);

			Element<HBox> todoCreateHBox = new Element<>(mainVBox, HBox.class);
			Element<TextField> textField = new Element<>(todoCreateHBox, TextField.class);
			textField.addBidirectionalBinding(TextField::textProperty, GenericList::getName);

			Element<Button> todosCreateButton = new Element<>(todoCreateHBox, Button.class);
			todosCreateButton.addBoot(Button::textProperty, "Create generic");
			todosCreateButton.addActionBinding(Button::onActionProperty, GenericList::create);

			Element<TableView> todoTableView = new Element<>(mainVBox, TableView.class);
			Function<TableView, ObservableValue<GenericWrapper>> function = t -> t.getSelectionModel().selectedItemProperty();

			todoTableView.addReversedBinding((Function) function, GenericList::getProperty);

			Element<GenericWrapper> todoTableItems = new Element<>(todoTableView, GenericWrapper.class, TableView<GenericWrapper>::getItems);
			todoTableItems.addForEachMetaBinding(GenericList::getGenerics);
			Element<Column> columnsTableItems = new Element<>(todoTableView, Column.class, TableView<GenericWrapper>::getColumns);
			columnsTableItems.addForEachMetaBinding(GenericList::getColumns);

			Element<HBox> hboxElement = new Element<HBox>(mainVBox, HBox.class);
			hboxElement.addBoot(HBox::spacingProperty, 5);
			Element<Button> buttonFlush = new Element<Button>(hboxElement, Button.class);
			buttonFlush.addActionBinding(Button::onActionProperty, GenericList::flush);
			buttonFlush.addBoot(Button::textProperty, "Flush");

			Element<Button> buttonClear = new Element<Button>(hboxElement, Button.class);
			buttonClear.addActionBinding(Button::onActionProperty, GenericList::clear);
			buttonClear.addBoot(Button::textProperty, "Clear");

			Element<Button> buttonMount = new Element<Button>(hboxElement, Button.class);
			buttonMount.addActionBinding(Button::onActionProperty, GenericList::mount);
			buttonMount.addBoot(Button::textProperty, "Mount");

			Element<Button> buttonUnmount = new Element<Button>(hboxElement, Button.class);
			buttonUnmount.addActionBinding(Button::onActionProperty, GenericList::unmount);
			buttonUnmount.addBoot(Button::textProperty, "Unmount");

			Callback<CellDataFeatures<GenericWrapper, String>, ObservableValue<String>> callback = features -> new SimpleObjectProperty<>(features.getValue().getObservable().getValue());
			// Callback<TableColumn<GenericWrapper, String>, TableCell<GenericWrapper, String>> callbackDelete = column -> new DeleteButtonCell<>(GenericWrapper::remove);

			Element<TableView> todoTableView2 = new Element<>(mainVBox, TableView.class);
			todoTableView2.addSelectorMetaBinding(GenericList::getProperty);

			Element<GenericWrapper> todoTableItems2 = new Element<>(todoTableView2, GenericWrapper.class, TableView<GenericWrapper>::getItems);
			todoTableItems2.addForEachMetaBinding(GenericWrapper::getGenericWrapperListInstances);
			Function<TableView<GenericWrapper>, ObservableList<?>> getItems = TableView::getItems;
			Function<TableView<?>, ObservableList<?>> getColumns = TableView::getColumns;

			Element<TableColumn> columnTodo = new Element<>(todoTableView2, TableColumn.class, getColumns);

			columnTodo.addBoot(TableColumn<GenericWrapper, String>::prefWidthProperty, 100);
			columnTodo.addBoot(TableColumn<GenericWrapper, String>::textProperty, "instance");
			columnTodo.addBoot(TableColumn<GenericWrapper, String>::cellValueFactoryProperty, callback);

			Element<TableColumn> columnDeleteTodo = new Element<>(todoTableView2, TableColumn.class, getColumns);

			columnDeleteTodo.addBoot(TableColumn<GenericWrapper, String>::prefWidthProperty, 150);
			columnDeleteTodo.addBoot(TableColumn<GenericWrapper, String>::textProperty, "Delete todo");
			columnDeleteTodo.addBoot(TableColumn<GenericWrapper, String>::cellValueFactoryProperty, callback);
			// columnDeleteTodo.addBoot(TableColumn<GenericWrapper, String>::cellFactoryProperty, callbackDelete);

			return sceneElt.apply(this, scene);
		}
	}

}
