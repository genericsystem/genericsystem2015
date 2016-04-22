package org.genericsystem.distributed.cacheonserver.ui.exemple;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.Color;
import org.genericsystem.distributed.cacheonserver.ui.list.TypeListModel;
import org.genericsystem.distributed.cacheonserver.ui.list.TypeListModel.TitleTypeListModel;
import org.genericsystem.distributed.cacheonserver.ui.table.TypeTableModel;
import org.genericsystem.distributed.cacheonserver.ui.table.title.TitleTypeTableModel;
import org.genericsystem.distributed.cacheonserver.ui.table.title.insertable.InsertTitleTypeTableModel;
import org.genericsystem.distributed.ui.models.GenericModel;
import org.genericsystem.kernel.Engine;

public class AppModel extends GenericModel {

	private final ObservableValue<TypeListModel> typeListModel;
	private final ObservableValue<TitleTypeListModel> titleTypeListModel;

	private final ObservableValue<TypeTableModel> typeTableModel;
	private final ObservableValue<TitleTypeTableModel> titleTypeTableModel;
	private final ObservableValue<InsertTitleTypeTableModel> insertableTitleTypeTableModel;
	private final ObservableValue<InsertTitleTypeTableModel> colorsInsertableTitleTypeTableModel;

	public AppModel(Engine engine, Generic type, ObservableList<Generic> attributes) {
		super(engine);
		typeListModel = new ReadOnlyObjectWrapper<>(new TypeListModel(type));
		titleTypeListModel = new ReadOnlyObjectWrapper<>(new TitleTypeListModel(type));
		typeTableModel = new ReadOnlyObjectWrapper<>(new TypeTableModel(type, attributes));
		titleTypeTableModel = new ReadOnlyObjectWrapper<>(new TitleTypeTableModel(type, attributes));
		insertableTitleTypeTableModel = new ReadOnlyObjectWrapper<>(new InsertTitleTypeTableModel(type, attributes));
		colorsInsertableTitleTypeTableModel = new ReadOnlyObjectWrapper<>(new InsertTitleTypeTableModel(engine.find(Color.class), FXCollections.emptyObservableList()));
	}

	public void flush() {
		getGeneric().getCurrentCache().flush();
	}

	public void cancel() {
		getGeneric().getCurrentCache().clear();
	}

	/*********************************************************************************************************************************/

	public ObservableValue<TypeListModel> getTypeListModel() {
		return typeListModel;
	}

	public ObservableValue<TitleTypeListModel> getTitleTypeListModel() {
		return titleTypeListModel;
	}

	public ObservableValue<TypeTableModel> getTypeTableModel() {
		return typeTableModel;
	}

	public ObservableValue<TitleTypeTableModel> getTitleTypeTableModel() {
		return titleTypeTableModel;
	}

	public ObservableValue<InsertTitleTypeTableModel> getInsertableTitleTypeTableModel() {
		return insertableTitleTypeTableModel;
	}

	public ObservableValue<InsertTitleTypeTableModel> getColorsInsertableTitleTypeTableModel() {
		return colorsInsertableTitleTypeTableModel;
	}
}
