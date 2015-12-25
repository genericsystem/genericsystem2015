package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.ui.Element;

public interface Builder {
	public void init(Element<?> parent);

	public abstract class ElementBuilder<ELEMENT, MODEL extends Model> implements Builder {

		protected abstract ObservableValue<ELEMENT> getFirstElement(MODEL model);

		// {
		// return new ReadOnlyObjectWrapper<>(new TextCellFirstRowBuilder<COL, U>().build(new RowModel<COL, U, String>(model.getFirstRowFirstColumnString(), model.getColumns(), model.getFirstRowExtractor(), model.getTableStyle())));
		// return new ReadOnlyObjectWrapper<>(new TextCellBuilder<>().build(model.getFirstColumnString(), getFirstCellStyle(model.getTableStyle())));
		//
		// }

		protected abstract ObservableList<ELEMENT> getElements(MODEL tableModel);
	}
}
