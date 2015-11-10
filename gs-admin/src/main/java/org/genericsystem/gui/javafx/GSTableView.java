package org.genericsystem.gui.javafx;

import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.util.Callback;

import org.genericsystem.common.Generic;
import org.genericsystem.gui.context.RootContext;
import org.genericsystem.gui.context.SubContext;

public class GSTableView extends TableView<Generic> {

	private RootContext rootContext;

	public GSTableView(RootContext rootContext) {
		this.rootContext = rootContext;
		initTable();
	}

	private void initTable() {
		super.setEditable(true);
		// GSTableColum<Generic, String> name = new GSTableColum<Generic, String>(new SubContext(rootContext, 0));

		setRowFactory(new Callback<TableView<Generic>, TableRow<Generic>>() {
			@Override
			public GSTableRow<Generic> call(TableView<Generic> param) {
				System.out.println("ggggg");
				return new GSTableRow<Generic>(new SubContext(rootContext, 0));
			}
		});
		// name.setCellFactory(TextFieldTableCell.forTableColumn());
		// name.setOnEditCommit(e -> System.out.println("ediiiiit"));
		// final TableColumn<Generic, ?> name = new TableColumn<>(rootContext.rootProperty.getValue().toString());
		// getColumns().add(name);
		// this.setItems(value);
		itemsProperty().set(rootContext.getCurrentCache().getDependenciesObservableList(rootContext.rootProperty.getValue()));
		// name.setCellValueFactory((g) -> new SimpleObjectProperty(g.getValue().toString()));
		// GSEditingCell<Generic, String> cell = new GSEditingCell<Generic, String>();
		// name.setCellValueFactory(cell.get);

	}

}
