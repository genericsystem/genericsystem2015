package org.genericsystem.todoApp;

import java.util.Arrays;
import java.util.Objects;
import java.util.function.Function;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.ListBinding;
import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.TableCell;
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
import org.genericsystem.ui.Binding;
import org.genericsystem.ui.Boot;
import org.genericsystem.ui.Element;

public class GenericList {

	private static final String path = System.getenv("HOME") + "/test/ObservableListChain";

	private static CocServer server;
	private static CocClientEngine engine;

	private static ObservableList<Generic> dependenciesObservableList;
	private static ObservableList<Generic> dependenciesObservableListFilterd = FXCollections.observableArrayList();
	Transformation<GenericWrapper, Generic> genericWrapperList;
	private ObservableList<GenericWrapper> genericWrapperListInstances = FXCollections.observableArrayList();

	private Property<String> name = new SimpleStringProperty();
	private ObservableList<Column> columns = FXCollections.observableArrayList(new Column(), new DeleteColumn());
	private ObservableValue<Number> height = new SimpleDoubleProperty(400);

	private Property<GenericWrapper> property = new SimpleObjectProperty<>();
	private Property<String> columnTitle = new SimpleStringProperty();

	public GenericList() throws InterruptedException {

		// cleanDirectory();
		server = new CocServer(new GSDeploymentOptions(Statics.ENGINE_VALUE, Statics.DEFAULT_PORT, path));
		server.start();

		engine = new CocClientEngine(Statics.ENGINE_VALUE, null, Statics.DEFAULT_PORT);

		dependenciesObservableList = engine.getCurrentCache().getObservableDependencies(engine);

		genericWrapperList = new Transformation<GenericWrapper, Generic>(dependenciesObservableList, generic -> new GenericWrapper(generic));
		genericWrapperListInstances = new ListBinding<GenericList.GenericWrapper>() {
			{
				super.bind(property);
			}

			@Override
			protected ObservableList<GenericWrapper> computeValue() {
				ObservableList<GenericWrapper> list = FXCollections.observableArrayList();
				if (property.getValue() != null) {

					columnTitle.setValue(property.getValue().generic.getValue().toString());
					dependenciesObservableListFilterd = FXCollections.observableArrayList(property.getValue().generic.getInstances().toList());
					dependenciesObservableListFilterd.forEach(gen -> list.add(new GenericWrapper(gen)));
				}
				return list;
			}
		};
	}

	public Property getName() {
		return name;
	}

	public ObservableList<GenericWrapper> getGenerics() {
		return genericWrapperList;
	}

	public ObservableValue<Number> getHeight() {
		return height;
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

	protected static class GenericWrapper {

		private Generic generic;
		private StringProperty stringProperty = new SimpleStringProperty();
		private ObservableValue<String> removeButtonTextProperty = Bindings.concat("Remove : ", stringProperty);

		public GenericWrapper(Generic generic) {
			this.generic = generic;
			stringProperty.set(Objects.toString(this.generic.getValue()));
		}

		public void remove() {
			generic.remove();
		}

		public ObservableValue<String> getObservable() {
			return stringProperty;
		}

		public ObservableValue<String> getRemoveButtonTextProperty() {
			return removeButtonTextProperty;
		}
	}

	public static class Column extends TableColumn<GenericWrapper, String> {
		public Column() {
			super("Todos");
			setMinWidth(130);
			setCellValueFactory(features -> new ReadOnlyObjectWrapper<String>(features.getValue().getObservable().getValue()));
		}
	}

	public static class DeleteColumn extends Column {
		public DeleteColumn() {
			setText("Delete");
			setMinWidth(130);
			setCellFactory(column -> new DeleteButtonCell<>(GenericWrapper::remove));
		}
	}

	public ObservableList<Column> getColumns() {
		return columns;
	}

	public ObservableList<GenericWrapper> getGenericWrapperListInstances() {
		return genericWrapperListInstances;
	}

	public Property<GenericWrapper> getProperty() {
		return property;
	}

	public Property<String> getColumnTitle() {
		return columnTitle;
	}

	public Node initTable() {

		Element<VBox> mainVBox = new Element<>(null, VBox.class);
		mainVBox.addBinding(Binding.bindProperty(VBox::prefHeightProperty, GenericList::getHeight));
		Element<HBox> todoCreateHBox = new Element<>(mainVBox, HBox.class);

		Element<TextField> textField = new Element<>(todoCreateHBox, TextField.class);
		textField.addBinding(Binding.bindInputText(TextField::textProperty, GenericList::getName));

		Element<Button> todosCreateButton = new Element<>(todoCreateHBox, Button.class);
		todosCreateButton.addBoots(Boot.setProperty(Button::textProperty, "Create generic"));
		todosCreateButton.addBinding(Binding.bindAction(Button::onActionProperty, GenericList::create));

		Element<TableView> todoTableView = new Element<>(mainVBox, TableView.class);
		Function<TableView, ReadOnlyObjectProperty> function = t -> t.getSelectionModel().selectedItemProperty();

		todoTableView.addBinding(Binding.bindReversedProperty((Function) function, GenericList::getProperty));
		Element<GenericWrapper> todoTableItems = new Element<>(todoTableView, GenericWrapper.class, TableView<GenericWrapper>::getItems, Arrays.asList(Binding.forEach(GenericList::getGenerics)));
		Element<Column> columnsTableItems = new Element<>(todoTableView, Column.class, TableView<GenericWrapper>::getColumns, Arrays.asList(Binding.forEach(GenericList::getColumns)));

		Element<HBox> hboxElement = new Element<HBox>(mainVBox, HBox.class);
		hboxElement.addBoots(Boot.setProperty(HBox::spacingProperty, 5));
		Element<Button> buttonFlush = new Element<Button>(hboxElement, Button.class, Binding.bindAction(Button::onActionProperty, GenericList::flush));
		buttonFlush.addBoots(Boot.setProperty(Button::textProperty, "Flush"));

		Element<Button> buttonClear = new Element<Button>(hboxElement, Button.class, Binding.bindAction(Button::onActionProperty, GenericList::clear));
		buttonClear.addBoots(Boot.setProperty(Button::textProperty, "Clear"));

		Element<Button> buttonMount = new Element<Button>(hboxElement, Button.class, Binding.bindAction(Button::onActionProperty, GenericList::mount));
		buttonMount.addBoots(Boot.setProperty(Button::textProperty, "Mount"));

		Element<Button> buttonUnmount = new Element<Button>(hboxElement, Button.class, Binding.bindAction(Button::onActionProperty, GenericList::unmount));
		buttonUnmount.addBoots(Boot.setProperty(Button::textProperty, "Unmount"));

		Callback<CellDataFeatures<GenericWrapper, String>, ObservableValue<String>> callback = features -> new SimpleObjectProperty<>(features.getValue().getObservable().getValue());
		Callback<TableColumn<GenericWrapper, String>, TableCell<GenericWrapper, String>> callbackDelete = column -> new DeleteButtonCell<>(GenericWrapper::remove);

		Element<TableView> todoTableView2 = new Element<>(mainVBox, TableView.class);
		Element<GenericWrapper> todoTableItems2 = new Element<>(todoTableView2, GenericWrapper.class, TableView<GenericWrapper>::getItems, Arrays.asList(Binding.forEach(GenericList::getGenericWrapperListInstances)));

		Function<TableView<GenericWrapper>, ObservableList<?>> getItems = TableView::getItems;
		Function<TableView<?>, ObservableList<?>> getColumns = TableView::getColumns;

		Element<TableColumn> columnTodo = new Element<>(todoTableView2, TableColumn.class, getColumns);
		columnTodo.addBinding(Binding.bindProperty(TableColumn::textProperty, GenericList::getColumnTitle));
		columnTodo.addBoots(Boot.setProperty(TableColumn<GenericWrapper, String>::prefWidthProperty, 100));
		columnTodo.addBoots(Boot.setProperty(TableColumn<GenericWrapper, String>::textProperty, "instance"));
		columnTodo.addBoots(Boot.setProperty(TableColumn<GenericWrapper, String>::cellValueFactoryProperty, callback));

		Element<TableColumn> columnDeleteTodo = new Element<>(todoTableView2, TableColumn.class, getColumns);

		columnDeleteTodo.addBoots(Boot.setProperty(TableColumn<GenericWrapper, String>::prefWidthProperty, 150));
		columnDeleteTodo.addBoots(Boot.setProperty(TableColumn<GenericWrapper, String>::textProperty, "Delete todo"));
		columnDeleteTodo.addBoots(Boot.setProperty(TableColumn<GenericWrapper, String>::cellValueFactoryProperty, callback), Boot.setProperty(TableColumn<GenericWrapper, String>::cellFactoryProperty, callbackDelete));

		return mainVBox.apply(this);
	}
}
