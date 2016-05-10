package org.genericsystem.ui.table;

import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import org.genericsystem.reactor.Model;

public class Stylable extends Model {
	private final ObservableValue<String> styleClass;

	public Stylable(ObservableValue<String> styleClass) {
		this.styleClass = styleClass;
	}

	public ObservableValue<String> getStyleClass() {
		return styleClass;
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
		private final ObservableList<Element> elements;
		private final ObservableValue<Element> lastElement;

		public Listable(ObservableValue<Element> firstElement, ObservableList<Element> elements, ObservableValue<Element> lastElement, ObservableValue<String> styleClass) {
			super(styleClass);
			this.firstElement = firstElement;
			this.elements = elements;
			this.lastElement = lastElement;
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