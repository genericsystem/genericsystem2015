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

public class InsertTitleTypeTableModel extends TitleTypeTableModel {

	private final ObservableValue<InsertRowModel> insertRowModel;

	public InsertTitleTypeTableModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor) {
		this(generic, GenericModel.SIMPLE_CLASS_EXTRACTOR, observableListExtractor, InstanceRowModel::new, CompositeModel<GenericModel>::new, GenericModel::new, TitleRowModel::new, GenericModel::new,
				(Function<MetaConf, InsertRowModel>) InsertRowModel::new, (BiFunction<Generic, Function<Generic, String>, InsertAttributeCellModel>) InsertAttributeCellModel::new);
	}

	public InsertTitleTypeTableModel(Generic generic, Function<Generic, String> stringExtractor, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<MetaConf, InstanceRowModel> rowBuilder,
			Function<MetaConf, CompositeModel<GenericModel>> cellBuilder, Function<Generic, GenericModel> subCellBuilder, Function<MetaConf, TitleRowModel> titleRowBuilder,
			BiFunction<Generic, Function<Generic, String>, GenericModel> titleCellBuilder, Function<MetaConf, InsertRowModel> insertRowBuilder, BiFunction<Generic, Function<Generic, String>, InsertAttributeCellModel> insertCellBuilder) {
		super(generic, stringExtractor, observableListExtractor, rowBuilder, cellBuilder, subCellBuilder, titleRowBuilder, titleCellBuilder);
		insertRowModel = new ReadOnlyObjectWrapper<>(insertRowBuilder.apply(new MetaConf(generic, observableListExtractor, attribute -> insertCellBuilder.apply(attribute, GenericModel.SIMPLE_CLASS_EXTRACTOR))));
	}

	public ObservableValue<InsertRowModel> getInsertRowModel() {
		return insertRowModel;
	}

}
