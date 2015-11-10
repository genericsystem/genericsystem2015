package org.genericsystem.newjavafx;

import java.util.Optional;

import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.Button;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextInputDialog;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import org.genericsystem.common.Generic;

public class Crud extends VBox {

	private RootContext currentContext;
	private Button changeRootProperty;
	private Button addButton;

	public Crud(RootContext ctx) {
		currentContext = ctx;
		TableView<Generic> tableViewEngine = new TableView<Generic>();
		tableViewEngine.itemsProperty().set(currentContext.observableGenericList);

		final TableColumn<Generic, String> name = new TableColumn<>(currentContext.rootProperty.getValue().toString());
		tableViewEngine.getColumns().add(name);

		name.setCellValueFactory((g) -> new SimpleObjectProperty(g.getValue().toString()));
		getChildren().add(tableViewEngine);

		changeRootProperty = new Button("Flush");
		addButton = new Button("Add");

		addButton.setOnAction(new EventHandler<ActionEvent>() {
			@Override
			public void handle(ActionEvent event) {
				currentContext.getEngine().addInstance("voiture");
			}
		});

		changeRootProperty.setOnAction(new EventHandler<ActionEvent>() {
			@Override
			public void handle(ActionEvent event) {
				// currentContext.rootProperty.set(null);
				currentContext.getEngine().getCurrentCache().flush();
				System.out.println("currentContext.rootProperty.set(null);");
			}
		});

		ContextMenu contextMenu = new ContextMenu();

		MenuItem item1 = new MenuItem("Add");
		item1.setOnAction(new EventHandler<ActionEvent>() {
			@Override
			public void handle(ActionEvent e) {
				TextInputDialog dialog = new TextInputDialog();
				dialog.setContentText("Name");
				Optional<String> value = dialog.showAndWait();
				if (value.isPresent()) {
					if (tableViewEngine.getSelectionModel().getSelectedItem() != null)
						tableViewEngine.getSelectionModel().getSelectedItem().addInstance(value.get());
					else
						currentContext.getEngine().addInstance(value.get());

				}
			}
		});
		MenuItem item2 = new MenuItem("Delete");
		item2.setOnAction(new EventHandler<ActionEvent>() {
			@Override
			public void handle(ActionEvent e) {
				tableViewEngine.getSelectionModel().getSelectedItem().remove();
			}
		});

		contextMenu.getItems().addAll(item1, item2);
		tableViewEngine.setContextMenu(contextMenu);

		HBox hbox = new HBox(5);
		hbox.getChildren().addAll(changeRootProperty, addButton);

		TableView<Generic> tableViewInstance = new TableView<Generic>();
		final TableColumn<Generic, String> nameInstance = new TableColumn<>("name");
		tableViewInstance.getColumns().add(nameInstance);

		tableViewEngine.getSelectionModel().selectedItemProperty().addListener(observable -> {
			Generic gen = tableViewEngine.getSelectionModel().getSelectedItem();
			if (gen != null) {
				ObservableList<Generic> listOb = currentContext.getEngine().getCurrentCache().getInstancesObservableList(gen);
				tableViewInstance.itemsProperty().set(listOb);
				// System.out.println("getValue*****************************");
				nameInstance.setCellValueFactory((g) -> new SimpleObjectProperty(g.getValue().toString()));
			}
		});

		// new InvalidationListener() {
		// @Override
		// public void invalidated(Observable observable) {
		// System.out.println("selectedItem");
		// }
		// });//
		getChildren().addAll(hbox, tableViewInstance);
	}
}
