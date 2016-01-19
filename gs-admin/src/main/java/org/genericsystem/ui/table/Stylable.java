package org.genericsystem.ui.table;

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
		public ObservableValue<String> firstRowLastCell = new ReadOnlyStringWrapper("firstrowlastcell");
		public ObservableValue<String> lastCell = new ReadOnlyStringWrapper("lastcell");
		public ObservableValue<String> secondCell = new ReadOnlyStringWrapper("secondCell");
		public ObservableValue<String> cell = new ReadOnlyStringWrapper("cell");
	}

	public static class Listable<Element> extends Stylable {

		private final ObservableValue<Element> firstElement;
		private final ObservableValue<Element> secondElement;
		private final ObservableList<Element> elements;
		private final ObservableValue<Element> lastElement;

		public Listable(ObservableValue<Element> firstElement, ObservableValue<Element> secondElement, ObservableList<Element> elements, ObservableValue<Element> lastElement, ObservableValue<String> styleClass) {
			super(styleClass);
			this.firstElement = firstElement;
			this.secondElement = secondElement;
			this.elements = elements;
			this.lastElement = lastElement;
		}

		public ObservableValue<Element> getSecondElement() {
			return secondElement;
		}

		public ObservableValue<Element> getFirstElement() {
			return firstElement;
		}

		public ObservableList<Element> getElements() {
			return elements;
		}

		public ObservableValue<Element> getLastElement() {
			return lastElement;
		}

	}
}