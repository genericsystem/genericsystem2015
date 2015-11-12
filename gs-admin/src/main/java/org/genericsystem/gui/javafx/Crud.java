package org.genericsystem.gui.javafx;

import javafx.scene.control.Button;
import javafx.scene.layout.VBox;

import org.genericsystem.gui.context.RootContext;

/**
 * @author Nicolas Feybesse
 *
 */
public class Crud extends VBox {

	private RootContext rootContext;

	private GSTableView tableView;
	private GSTableView tab;
	private RootContext rc;

	public Crud(RootContext rootContext) {
		this.rootContext = rootContext;
		// rc = rootContext;
		tableView = new GSTableView(rootContext);
		getChildren().add(tableView);
		Button buttonAdd = new Button("add");
		buttonAdd.setOnAction(e -> {
			tableView.getSelectionModel().getSelectedItem().observableGeneric.getValue().remove();
		});

		rc = new RootContext(tableView.getItems().get(0).observableGeneric.getValue());
		tableView.getSelectionModel().selectedItemProperty().addListener(observable -> {
			rc.rootProperty.set(tableView.getSelectionModel().getSelectedItem().observableGeneric.getValue());
		});

		tab = new GSTableView(rc);
		getChildren().addAll(tab, buttonAdd);
	}

	public RootContext getContext() {
		return rootContext;
	}
}
