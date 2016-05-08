package org.genericsystem.distributed.cacheonserver.ui.exemple;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonserver.ui.table.TypeTableModel;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.kernel.Engine;

public class AppModel extends CompositeModel {

	private final ObservableValue<CompositeModel> typeListModel;
	private final ObservableValue<CompositeModel> titleTypeListModel;

	private final ObservableValue<TypeTableModel> typeTableModel;

	// private final ObservableValue<TitleTypeTableModel> titleTypeTableModel;
	// private final ObservableValue<InsertTitleTypeTableModel> insertableTitleTypeTableModel;
	// private final ObservableValue<InsertTitleTypeTableModel> colorsInsertableTitleTypeTableModel;

	public AppModel(Engine engine, Generic type) {
		super(new Generic[] { engine }, StringExtractor.SIMPLE_CLASS_EXTRACTOR);
		typeListModel = new ReadOnlyObjectWrapper<>(new CompositeModel<>(new Generic[] { type }, StringExtractor.SIMPLE_CLASS_EXTRACTOR));
		titleTypeListModel = new ReadOnlyObjectWrapper<>(new CompositeModel<>(new Generic[] { type }, g -> StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(g) + "(s) Management"));
		typeTableModel = new ReadOnlyObjectWrapper<>(new TypeTableModel(new Generic[] { type }, g -> StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(g) + "(s) Management"));

		// titleTypeTableModel = new ReadOnlyObjectWrapper<>(new TitleTypeTableModel(type, typ -> attributes));
		// insertableTitleTypeTableModel = new ReadOnlyObjectWrapper<>(new InsertTitleTypeTableModel(type, typ -> attributes));
		// colorsInsertableTitleTypeTableModel = new ReadOnlyObjectWrapper<>(new InsertTitleTypeTableModel(engine.find(Color.class), typ -> FXCollections.emptyObservableList()));
	}

	public void flush() {
		getGeneric().getCurrentCache().flush();
	}

	public void cancel() {
		getGeneric().getCurrentCache().clear();
	}

	/*********************************************************************************************************************************/

	public ObservableValue<CompositeModel> getTypeListModel() {
		return typeListModel;
	}

	public ObservableValue<CompositeModel> getTitleTypeListModel() {
		return titleTypeListModel;
	}

	public ObservableValue<TypeTableModel> getTypeTableModel() {
		return typeTableModel;
	}

	// public ObservableValue<TitleTypeTableModel> getTitleTypeTableModel() {
	// return titleTypeTableModel;
	// }
	//
	// public ObservableValue<InsertTitleTypeTableModel> getInsertableTitleTypeTableModel() {
	// return insertableTitleTypeTableModel;
	// }

	// public ObservableValue<InsertTitleTypeTableModel> getColorsInsertableTitleTypeTableModel() {
	// return colorsInsertableTitleTypeTableModel;
	// }
}
