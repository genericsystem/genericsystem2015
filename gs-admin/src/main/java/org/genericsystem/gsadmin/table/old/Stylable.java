package org.genericsystem.gsadmin.table.old;

import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

public class Stylable {
	private ObservableValue<String> styleClass;

	public Stylable(ObservableValue<String> styleClass) {
		this.styleClass = styleClass;
	}

	public ObservableValue<String> getStyleClass() {
		return styleClass;
	}

	public void setStyleClass(ObservableValue<String> styleClass) {
		this.styleClass = styleClass;
	}

	public static class TableStyle {
		public ObservableValue<String> table = new ReadOnlyStringWrapper("table");
		public ObservableValue<String> firstRow = new ReadOnlyStringWrapper("firstrow");
		public ObservableValue<String> row = new ReadOnlyStringWrapper("row");
		public ObservableValue<String> firstRowCell = new ReadOnlyStringWrapper("firstrowcell");
		public ObservableValue<String> firstRowFirstCell = new ReadOnlyStringWrapper("firstrowfirstcell");
		public ObservableValue<String> firstCell = new ReadOnlyStringWrapper("firstcell");
		public ObservableValue<String> cell = new ReadOnlyStringWrapper("cell");
	}

	public static class Listable<Element> extends Stylable {

		private final ObservableValue<Element> firstElement;
		private final ObservableList<Element> elements;

		public Listable(ObservableValue<Element> firstElement, ObservableList<Element> elements, ObservableValue<String> styleClass) {
			super(styleClass);
			this.firstElement = firstElement;
			this.elements = elements;
		}

		public ObservableValue<Element> getFirstElement() {
			return firstElement;
		}

		public ObservableList<Element> getElements() {
			return elements;
		}

	}
}