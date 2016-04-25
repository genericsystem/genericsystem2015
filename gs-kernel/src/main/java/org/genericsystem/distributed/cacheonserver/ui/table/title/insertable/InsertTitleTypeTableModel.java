package org.genericsystem.distributed.cacheonserver.ui.table.title.insertable;

import java.util.function.BiFunction;
import java.util.function.Function;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonserver.ui.table.InstanceRowModel;
import org.genericsystem.distributed.cacheonserver.ui.table.title.TitleRowModel;
import org.genericsystem.distributed.cacheonserver.ui.table.title.TitleTypeTableModel;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;
import org.genericsystem.distributed.ui.models.TriFunction;

public class InsertTitleTypeTableModel extends TitleTypeTableModel {

	private final ObservableValue<InsertRowModel> insertRowModel;

	public InsertTitleTypeTableModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor) {
		this(generic, observableListExtractor, InstanceRowModel::new, CompositeModel<GenericModel>::new, GenericModel::new, TitleRowModel::new, GenericModel::new, InsertRowModel::new, InsertAttributeCellModel::new);
	}

	public InsertTitleTypeTableModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor,
			TriFunction<Generic, Function<Generic, ObservableList<Generic>>, Function<Generic, CompositeModel<GenericModel>>, InstanceRowModel> rowBuilder,
			TriFunction<Generic, Function<Generic, ObservableList<Generic>>, Function<Generic, GenericModel>, CompositeModel<GenericModel>> cellBuilder, Function<Generic, GenericModel> subCellBuilder,
			TriFunction<Generic, Function<Generic, ObservableList<Generic>>, Function<Generic, GenericModel>, TitleRowModel> titleRowBuilder, BiFunction<Generic, Function<Generic, String>, GenericModel> titleCellBuilder,
			TriFunction<Generic, Function<Generic, ObservableList<Generic>>, Function<Generic, InsertAttributeCellModel>, InsertRowModel> insertRowBuilder, Function<Generic, InsertAttributeCellModel> insertCellBuilder) {
		super(generic, observableListExtractor, rowBuilder, cellBuilder, subCellBuilder, titleRowBuilder, titleCellBuilder);
		insertRowModel = new ReadOnlyObjectWrapper<>(insertRowBuilder.apply(generic, observableListExtractor, insertCellBuilder));
	}

	public ObservableValue<InsertRowModel> getInsertRowModel() {
		return insertRowModel;
	}

}
