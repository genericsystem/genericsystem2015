package org.genericsystem.gui.javafx;

import java.util.Objects;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;

import org.genericsystem.gui.context.RootContext;
import org.genericsystem.gui.context.SubContext;

public class GSTableView extends TableView<SubContext> {

	private RootContext rootContext;

	public GSTableView(RootContext rootContext) {
		this.rootContext = rootContext;
		initTable();
	}

	private void initTable() {
		final TableColumn<SubContext, ?> tabColumn = new TableColumn<>(rootContext.rootProperty.getValue().toString());
		getColumns().add(tabColumn);
		tabColumn.setCellValueFactory((g) -> new ReadOnlyObjectWrapper<String>(Objects.toString(g.getValue().observableGeneric.getValue())));
		itemsProperty().set(rootContext.observableSubContextList);

		// SubContext sub = (SubContext) rootContext.observableSubContextList.get(0);
		// TableRow<Generic> tbr = new TableRow<Generic>();
		// name.setCellFactory(Te
		// name.setOnEditCommit(e -> System.out.println("ediiiiit"));
		// this.setItems(value);
		// itemsProperty().set(rootContext.getCurrentCache().getDependenciesObservableList(rootContext.rootProperty.getValue()));
		// new SimpleObjectProperty(g.getValue().toString())
		// GSEditingCell<Generic, String> cell = new GSEditingCell<Generic, String>();
		// name.setCellValueFactory(cell.get);

	}
}
