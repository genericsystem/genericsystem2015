package org.genericsystem.gui.javafx;

import java.util.Objects;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;

import org.genericsystem.gui.context.GenericContext;
import org.genericsystem.gui.context.TableViewContext;

public class GSTableView extends TableView<GenericContext> {

	private TableViewContext tableViewContext;

	public GSTableView(TableViewContext rootContext) {
		this.tableViewContext = rootContext;

		TableColumn<GenericContext, ?> tabColumn = new TableColumn<>();
		tabColumn.textProperty().bind(tableViewContext.columnTitle);
		tabColumn.setCellValueFactory((g) -> new ReadOnlyObjectWrapper<String>(Objects.toString(g.getValue().genericProperty.getValue().getValue())));

		getColumns().add(tabColumn);
		itemsProperty().set(tableViewContext.subContexObservableList);

	}
}
