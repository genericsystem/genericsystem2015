package org.genericsystem.distributed.cacheonserver.ui.exemple;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonserver.ui.list.TitleGenericCompositeModel;
import org.genericsystem.distributed.cacheonserver.ui.table.InstanceRowModel;
import org.genericsystem.distributed.cacheonserver.ui.table.TypeTableModel;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;
import org.genericsystem.distributed.ui.models.TableConfig;
import org.genericsystem.kernel.Engine;

public class AppModel extends GenericModel {

	private final ObservableValue<CompositeModel<GenericModel>> typeListModel;
	private final ObservableValue<TitleGenericCompositeModel> titleTypeListModel;

	private final ObservableValue<TypeTableModel> typeTableModel;

	// private final ObservableValue<TitleTypeTableModel> titleTypeTableModel;
	// private final ObservableValue<InsertTitleTypeTableModel> insertableTitleTypeTableModel;
	// private final ObservableValue<InsertTitleTypeTableModel> colorsInsertableTitleTypeTableModel;

	public AppModel(Engine engine, Generic type, ObservableList<Generic> attributes) {
		super(new Generic[] { engine });

		typeListModel = new ReadOnlyObjectWrapper<>(new CompositeModel<>(new Generic[] { type }, generics -> generics[0].getObservableSubInstances()));
		titleTypeListModel = new ReadOnlyObjectWrapper<>(new TitleGenericCompositeModel(new Generic[] { type }, g -> GenericModel.SIMPLE_CLASS_EXTRACTOR.apply(g) + "(s) Management", generics -> generics[0].getObservableSubInstances()));
		typeTableModel = new ReadOnlyObjectWrapper<>(buildTableModel(type, typ -> attributes));

		// titleTypeTableModel = new ReadOnlyObjectWrapper<>(new TitleTypeTableModel(type, typ -> attributes));
		// insertableTitleTypeTableModel = new ReadOnlyObjectWrapper<>(new InsertTitleTypeTableModel(type, typ -> attributes));
		// colorsInsertableTitleTypeTableModel = new ReadOnlyObjectWrapper<>(new InsertTitleTypeTableModel(engine.find(Color.class), typ -> FXCollections.emptyObservableList()));
	}

	private <T extends CompositeModel<?>> T buildTableModel(Generic generic, ObservableListExtractor attributesExtractor) {
		TableConfig confs = new TableConfig();
		confs.pushStep(GenericModel.SIMPLE_CLASS_EXTRACTOR, generics -> generics[0].getObservableSubInstances(), TypeTableModel::new);
		confs.pushStep(GenericModel.SIMPLE_CLASS_EXTRACTOR, attributesExtractor, InstanceRowModel::new);
		confs.pushStep_(GenericModel.SIMPLE_CLASS_EXTRACTOR, generics -> generics[1].getObservableHolders(generics[0]));
		confs.pushSimpleStep();
		return confs.build(generic);
	}

	public void flush() {
		getGeneric().getCurrentCache().flush();
	}

	public void cancel() {
		getGeneric().getCurrentCache().clear();
	}

	/*********************************************************************************************************************************/

	public ObservableValue<CompositeModel<GenericModel>> getTypeListModel() {
		return typeListModel;
	}

	public ObservableValue<TitleGenericCompositeModel> getTitleTypeListModel() {
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
