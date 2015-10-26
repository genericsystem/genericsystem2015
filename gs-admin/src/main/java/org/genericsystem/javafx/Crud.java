package org.genericsystem.javafx;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.geometry.Insets;
import javafx.scene.layout.VBox;

import org.genericsystem.admin.UiFunctions;

/**
 * @author Nicolas Feybesse
 *
 */
public class Crud<G> extends VBox {

	public Crud(ObjectProperty<G> metaTypeProperty, UiFunctions<G> gsFunctions) {
		setPadding(new Insets(10, 10, 10, 10));
		ObjectProperty<G> typeProperty = new SimpleObjectProperty<G>();
		ObjectProperty<G> panelProperty = new SimpleObjectProperty<G>();

		InstancesTableView<G> table = new InstancesTableView<>(metaTypeProperty, gsFunctions);
		InstancesTableView<G> table2 = new InstancesTableView<>(typeProperty, gsFunctions);

		CommandsPanel<G> commandPanel = new CommandsPanel(metaTypeProperty, gsFunctions);

		getChildren().addAll(table, table2, commandPanel);

		//
		// HBox hboxPane = new HBox(5);
		// hboxPane.setPadding(new Insets(15, 12, 15, 12));
		// hboxPane.getChildren().addAll(new Button("Flush"), new Button("Cancel"));
		// getChildren().add(hboxPane);

		typeProperty.bind(table.getSelectionModel().selectedItemProperty());
	}
}
