package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.ui.Element;

public interface Builder {
	public void init(Element<?> parent);

	public abstract class ElementBuilder<ELEMENT, SUBELEMENT, MODEL extends Model> implements Builder {

		protected abstract ELEMENT build(MODEL model);

		protected abstract ObservableValue<String> getStyle(MODEL model);

		protected abstract ObservableValue<SUBELEMENT> getFirstElement(MODEL model);

		protected abstract ObservableList<SUBELEMENT> getElements(MODEL tableModel);
	}
}
