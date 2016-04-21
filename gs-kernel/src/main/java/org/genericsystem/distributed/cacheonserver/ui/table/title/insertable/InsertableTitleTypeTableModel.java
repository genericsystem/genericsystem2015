package org.genericsystem.distributed.cacheonserver.ui.table.title.insertable;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonserver.ui.table.title.TitleTypeTableModel;

public class InsertableTitleTypeTableModel extends TitleTypeTableModel {

	private final ObservableValue<InsertRowModel> insertRowModel;

	public InsertableTitleTypeTableModel(Generic type, ObservableList<Generic> attributes) {
		super(type, attributes);
		insertRowModel = new ReadOnlyObjectWrapper<>(getInsertRowBuilder().apply(type, transform(attributes, getInsertCellBuilder())));
	}

	public ObservableValue<InsertRowModel> getInsertRowModel() {
		return insertRowModel;
	}

}
