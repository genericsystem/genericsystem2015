package org.genericsystem.gui.javafx;

import javafx.beans.property.SimpleObjectProperty;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.TextFieldTableCell;

import org.genericsystem.common.Generic;
import org.genericsystem.gui.context.RootContext;

public class GSTableView extends TableView<Generic> {

	private RootContext rootContext;

	public GSTableView(RootContext rootContext) {
		this.rootContext = rootContext;
		initTable();
	}

	private void initTable() {
		super.setEditable(true);
		GSTableColum<Generic, String> name = new GSTableColum<Generic, String>(rootContext);

		name.setCellFactory(TextFieldTableCell.forTableColumn());
		name.setOnEditCommit(e -> System.out.println("ediiiiit"));
		// final TableColumn<Generic, ?> name = new TableColumn<>(rootContext.rootProperty.getValue().toString());
		getColumns().add(name);
		itemsProperty().set(rootContext.observableGenericList);
		name.setCellValueFactory((g) -> new SimpleObjectProperty(g.getValue().toString()));
		// GSEditingCell<Generic, String> cell = new GSEditingCell<Generic, String>();

		// name.setCellValueFactory(cell.get);

	}

}
