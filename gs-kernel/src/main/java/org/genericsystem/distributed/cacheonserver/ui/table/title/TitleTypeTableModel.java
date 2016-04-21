package org.genericsystem.distributed.cacheonserver.ui.table.title;

import java.util.function.BiFunction;
import java.util.function.Function;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonserver.ui.table.TypeTableModel;
import org.genericsystem.distributed.ui.models.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */
public class TitleTypeTableModel extends TypeTableModel {

	private final ObservableValue<TitleRowModel> titleRowModel;

	public TitleTypeTableModel(Generic type, ObservableList<Generic> attributes) {
		super(type, attributes);
		Function<Generic, GenericModel> titleCellBuilder = attribute -> getTitleCellBuilder().apply(attribute, getTitleCellStringExtractor());
		titleRowModel = new ReadOnlyObjectWrapper<>(getTitleRowBuilder().apply(type, transform(attributes, titleCellBuilder)));
	}

	public ObservableValue<TitleRowModel> getTitleRowModel() {
		return titleRowModel;
	}

	protected BiFunction<Generic, ObservableList<GenericModel>, TitleRowModel> getTitleRowBuilder() {
		return TitleRowModel::new;
	}

	protected Function<Generic, String> getTitleCellStringExtractor() {
		return STRING_EXTRACTOR;
	}

}
