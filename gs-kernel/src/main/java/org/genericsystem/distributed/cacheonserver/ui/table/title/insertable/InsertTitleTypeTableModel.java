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
				(Function<Step, InsertRowModel>) InsertRowModel::new, (BiFunction<Generic, Function<Generic, String>, InsertAttributeCellModel>) InsertAttributeCellModel::new);
	}

	public InsertTitleTypeTableModel(Generic generic, Function<Generic, String> stringExtractor, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Step, InstanceRowModel> rowBuilder,
			Function<Step, CompositeModel<GenericModel>> cellBuilder, Function<Generic, GenericModel> subCellBuilder, Function<Step, TitleRowModel> titleRowBuilder,
			BiFunction<Generic, Function<Generic, String>, GenericModel> titleCellBuilder, Function<Step, InsertRowModel> insertRowBuilder, BiFunction<Generic, Function<Generic, String>, InsertAttributeCellModel> insertCellBuilder) {
		super(generic, stringExtractor, observableListExtractor, rowBuilder, cellBuilder, subCellBuilder, titleRowBuilder, titleCellBuilder);
		insertRowModel = new ReadOnlyObjectWrapper<>(insertRowBuilder.apply(new Step(generic, observableListExtractor, attribute -> insertCellBuilder.apply(attribute, GenericModel.SIMPLE_CLASS_EXTRACTOR))));
	}

	// public static TypeTableModel build(Generic generic, Function<Generic[], ObservableList<Generic>> attributesExtractor) {
	// List<MetaConf> confs = new ArrayList<>();
	// confs.add(new MetaConf(GenericModel.SIMPLE_CLASS_EXTRACTOR, null, GenericModel::new));
	// confs.add(new MetaConf(GenericModel.SIMPLE_CLASS_EXTRACTOR, generics -> generics[0].getObservableHolders(generics[1]), GenericCompositeModel::new));
	// confs.add(new MetaConf(GenericModel.SIMPLE_CLASS_EXTRACTOR, attributesExtractor, InstanceRowModel::new));
	// confs.add(new MetaConf(GenericModel.SIMPLE_CLASS_EXTRACTOR, generics -> generics[0].getObservableSubInstances(), TypeTableModel::new));
	// return (TypeTableModel) getBuilder(confs).apply(generic);
	// }

	public ObservableValue<InsertRowModel> getInsertRowModel() {
		return insertRowModel;
	}

}
