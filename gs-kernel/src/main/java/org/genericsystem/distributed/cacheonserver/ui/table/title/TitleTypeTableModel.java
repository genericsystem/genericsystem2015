package org.genericsystem.distributed.cacheonserver.ui.table.title;

import java.util.function.BiFunction;
import java.util.function.Function;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonserver.ui.table.InstanceRowModel;
import org.genericsystem.distributed.cacheonserver.ui.table.TypeTableModel;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;
import org.genericsystem.distributed.ui.models.TriFunction;

/**
 * @author Nicolas Feybesse
 *
 */
public class TitleTypeTableModel extends TypeTableModel {

	private final ObservableValue<TitleRowModel> titleRowModel;

	public TitleTypeTableModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor) {
		this(generic, observableListExtractor, InstanceRowModel::new, CompositeModel<GenericModel>::new, GenericModel::new, TitleRowModel::new, GenericModel::new);
	}

	public TitleTypeTableModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor,
			TriFunction<Generic, Function<Generic, ObservableList<Generic>>, Function<Generic, CompositeModel<GenericModel>>, InstanceRowModel> rowBuilder,
			TriFunction<Generic, Function<Generic, ObservableList<Generic>>, Function<Generic, GenericModel>, CompositeModel<GenericModel>> cellBuilder, Function<Generic, GenericModel> subCellBuilder,
			TriFunction<Generic, Function<Generic, ObservableList<Generic>>, Function<Generic, GenericModel>, TitleRowModel> titleRowBuilder, BiFunction<Generic, Function<Generic, String>, GenericModel> titleCellBuilder) {
		super(generic, observableListExtractor, rowBuilder, cellBuilder, subCellBuilder);
		titleRowModel = new ReadOnlyObjectWrapper<>(titleRowBuilder.apply(generic, observableListExtractor, attribute -> titleCellBuilder.apply(attribute, getTitleCellStringExtractor())));

	}

	public ObservableValue<TitleRowModel> getTitleRowModel() {
		return titleRowModel;
	}

	protected Function<Generic, String> getTitleCellStringExtractor() {
		return GenericModel.SIMPLE_CLASS_EXTRACTOR;
	}

}
