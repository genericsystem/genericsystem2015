package org.genericsystem.todoApp;

import java.util.Arrays;
import java.util.Objects;
import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
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
	Transformation<GenericWrapper, Generic> genericWrapperList;

	private Property<String> name = new SimpleStringProperty();
	// private ObservableList<GenericWrapper> todos = FXCollections.observableArrayList();
	private ObservableList<Column> columns = FXCollections.observableArrayList(new Column(), new DeleteColumn());
	private ObservableValue<Number> height = new SimpleDoubleProperty(400);

	// private StringProperty name = new SimpleStringProperty();
	// private ObservableValue<String> createButtonTextProperty = new SimpleStringProperty("Create Generic");
	// private ObservableValue<String> flushButtonTextProperty = new SimpleStringProperty("Flush");
	// private ObservableValue<String> clearButtonTextProperty = new SimpleStringProperty("Clear");
	// private ObservableValue<String> mountButtonTextProperty = new SimpleStringProperty("Mount");
	// private ObservableValue<String> unmountButtonTextProperty = new SimpleStringProperty("Unmount");
	// private ObservableValue<Number> height = new SimpleDoubleProperty(700);

	public GenericList() throws InterruptedException {

		// cleanDirectory();
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

	// public ObservableValue<String> getCreateButtonTextProperty() {
	// return createButtonTextProperty;
	// }
	//
	// public ObservableValue<String> getFlushButtonTextProperty() {
	// return flushButtonTextProperty;
	// }
	//
	// public ObservableValue<String> getClearButtonTextProperty() {
	// return clearButtonTextProperty;
	// }
	//
	// public ObservableValue<String> getMountButtonTextProperty() {
	// return mountButtonTextProperty;
	// }
	//
	// public ObservableValue<String> getUnmountButtonTextProperty() {
	// return unmountButtonTextProperty;
	// }

	public ObservableValue<Number> getHeight() {
		return height;
	}

	// private void cleanDirectory() {
	// File file = new File(path);
	// if (file.exists())
	// public ObservableValue<String> getCreateButtonTextProperty() {
	// return createButtonTextProperty;
	// }
	//
	// for (File f : file.listFiles()) {
	// if (!".lock".equals(f.getName()))
	// f.delete();
	// }
	// }

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

	public void remove(GenericWrapper genericWrapper) {
		genericWrapper.remove();
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
			setCellFactory(column -> new DeleteButtonCell<>());
		}
	}

	public ObservableList<Column> getColumns() {
		return columns;
	}

	// public Node init() {
	// Element mainVBox = new Element(null, VBox.class, Binding.bindProperty(VBox::prefHeightProperty, GenericList::getHeight));
	// Element todoCreateHBox = new Element(mainVBox, HBox.class);
	// Element todosCreatLabel = new Element(todoCreateHBox, TextField.class, Binding.bindInputText(TextField::textProperty, GenericList::getName));
	// Element todosCreateButton = new Element(todoCreateHBox, Button.class, Binding.bindProperty(Button::textProperty, GenericList::getCreateButtonTextProperty), Binding.bindAction(Button::onActionProperty, GenericList::create));
	//
	// Element todoHBox = new Element(mainVBox, HBox.class, VBox::getChildren, Arrays.asList(Binding.forEach(GenericList::getGenerics)));
	// Element todoLabel = new Element(todoHBox, Label.class, Binding.bindProperty(Label::textProperty, GenericWrapper::getObservable));
	// Element todoRemoveButton = new Element(todoHBox, Button.class, Binding.bindAction(Button::onActionProperty, GenericList::remove, GenericWrapper.class), Binding.bindProperty(Button::textProperty, GenericWrapper::getRemoveButtonTextProperty));
	//
	// return (Node) mainVBox.apply(this).getNode();
	// }

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
		Element<GenericWrapper> todoTableItems = new Element<>(todoTableView, GenericWrapper.class, TableView<GenericWrapper>::getItems, Arrays.asList(Binding.forEach(GenericList::getGenerics)));
		Element<Column> columnsTableItems = new Element<>(todoTableView, Column.class, TableView<GenericWrapper>::getColumns, Arrays.asList(Binding.forEach(GenericList::getColumns)));

		return mainVBox.apply(this);
	}
}
