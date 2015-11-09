package org.genericsystem.newjavafx;

import javafx.beans.property.SimpleObjectProperty;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.Button;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.layout.VBox;

import org.genericsystem.common.Generic;

public class Crud extends VBox {

	private RootContext currentContext;
	private Button changeRootProperty;
	private Button addButton;

	public Crud(RootContext ctx) {
		currentContext = ctx;
		TableView<Generic> table = new TableView<Generic>();
		table.itemsProperty().set(currentContext.observableGenericList);
		final TableColumn<Generic, String> name = new TableColumn<>(currentContext.rootProperty.getValue().toString());
		table.getColumns().add(name);

		name.setCellValueFactory((g) -> new SimpleObjectProperty(g.getValue().toString()));
		getChildren().add(table);

		changeRootProperty = new Button("reset");

		addButton = new Button("Add");

		addButton.setOnAction(new EventHandler<ActionEvent>() {
			@Override
			public void handle(ActionEvent event) {
				currentContext.getCache().addInstance("voiture");
			}
		});

		changeRootProperty.setOnAction(new EventHandler<ActionEvent>() {
			@Override
			public void handle(ActionEvent event) {
				currentContext.rootProperty.set(null);
				System.out.println("currentContext.rootProperty.set(null);");
			}
		});

		getChildren().addAll(changeRootProperty, addButton);
	}
}
