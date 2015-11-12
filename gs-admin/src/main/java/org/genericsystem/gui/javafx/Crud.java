package org.genericsystem.gui.javafx;

import javafx.scene.control.Button;
import javafx.scene.layout.VBox;

import org.genericsystem.gui.context.RootContext;

/**
 * @author Nicolas Feybesse
 *
 */
public class Crud extends VBox {

	private final RootContext rootContext;
	private GSTableView tableView;

	public Crud(RootContext rootContext) {
		this.rootContext = rootContext;
		tableView = new GSTableView(rootContext);
		getChildren().add(tableView);

		Button b = new Button("add");
		b.setOnAction(e -> {
			tableView.getSelectionModel().getSelectedItem().observableGeneric.getValue().remove();
		});
		getChildren().add(b);
	}

	public RootContext getContext() {
		return rootContext;
	}
}
