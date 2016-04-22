package org.genericsystem.distributed.cacheonserver.ui.table.title.insertable;

import java.util.function.BiFunction;
import java.util.function.Function;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonserver.ui.table.title.TitleTypeTableModel;

public class InsertTitleTypeTableModel extends TitleTypeTableModel {

	private final ObservableValue<InsertRowModel> insertRowModel;

	public InsertTitleTypeTableModel(Generic type, ObservableList<Generic> attributes) {
		super(type, attributes);
		insertRowModel = new ReadOnlyObjectWrapper<>(getInsertRowBuilder().apply(type, transform(attributes, getInsertCellBuilder())));
	}

	public ObservableValue<InsertRowModel> getInsertRowModel() {
		return insertRowModel;
	}

	protected BiFunction<Generic, ObservableList<InsertAttributeCellModel>, InsertRowModel> getInsertRowBuilder() {
		return InsertRowModel::new;
	}

	protected Function<Generic, InsertAttributeCellModel> getInsertCellBuilder() {
		return InsertAttributeCellModel::new;
	}

}
