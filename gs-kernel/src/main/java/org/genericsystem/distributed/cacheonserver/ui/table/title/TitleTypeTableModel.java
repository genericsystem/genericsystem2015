package org.genericsystem.distributed.cacheonserver.ui.table.title;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonserver.ui.table.TypeTableModel;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;
import org.genericsystem.distributed.ui.models.TableConfig;

/**
 * @author Nicolas Feybesse
 *
 */
public class TitleTypeTableModel extends TypeTableModel {

	private final ObservableValue<TitleRowModel> titleRowModel;

	public TitleTypeTableModel(Generic[] generics, StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, Builder<?> builder) {
		super(generics, stringExtractor, observableListExtractor, builder);
		titleRowModel = new ReadOnlyObjectWrapper<>(buildTableModel(generics[0]));
	}

	private <T extends CompositeModel<?>> T buildTableModel(Generic generic) {
		TableConfig confs = new TableConfig();
		confs.pushStep(GenericModel.SIMPLE_CLASS_EXTRACTOR, observableListExtractor, TitleTypeTableModel::new);
		confs.pushSimpleStep();
		return confs.build(generic);
	}

	public ObservableValue<TitleRowModel> getTitleRowModel() {
		return titleRowModel;
	}

}
