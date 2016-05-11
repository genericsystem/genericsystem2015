package org.genericsystem.exemple;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import org.genericsystem.common.Generic;
import org.genericsystem.kernel.Engine;
import org.genericsystem.reactor.CompositeModel;

public class AppModel extends CompositeModel {

	private final Property<CompositeModel> typeListModel = new SimpleObjectProperty<>();;
	private final Property<CompositeModel> titleTypeListModel = new SimpleObjectProperty<>();;

	private final Property<CompositeModel> typeTableModel = new SimpleObjectProperty<>();;

	// private final ObservableValue<TitleTypeTableModel> titleTypeTableModel;
	// private final ObservableValue<InsertTitleTypeTableModel> insertableTitleTypeTableModel;
	// private final ObservableValue<InsertTitleTypeTableModel> colorsInsertableTitleTypeTableModel;

	public AppModel(Engine engine) {
		super(new Generic[] { engine }, StringExtractor.SIMPLE_CLASS_EXTRACTOR);
	}

	public void flush() {
		getGeneric().getCurrentCache().flush();
	}

	public void cancel() {
		getGeneric().getCurrentCache().clear();
	}

	/*********************************************************************************************************************************/

	public Property<CompositeModel> getTypeListModel() {
		return typeListModel;
	}

	public Property<CompositeModel> getTitleTypeListModel() {
		return titleTypeListModel;
	}

	public Property<CompositeModel> getTypeTableModel() {
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
