package org.genericsystem.ui.components;

import java.util.function.Function;

import javafx.collections.ObservableList;
import javafx.scene.control.TableView;

import org.genericsystem.distributed.ui.Element;

public class GSTableItem<T> extends Element<T> {

	private static Function<TableView<?>, ObservableList<?>> getItems = TableView::getItems;

	public GSTableItem(Element parent, Class<T> nodeClass) {
		super(parent, nodeClass, getItems);
	}
}
