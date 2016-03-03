package org.genericsystem.ui.components;

import java.util.List;
import java.util.function.Function;

import javafx.scene.control.TableView;

import org.genericsystem.distributed.ui.Element;

public class GSTableItem<T> extends Element<T> {

	private static Function<TableView<?>, List<?>> getItems = TableView::getItems;

	public GSTableItem(Element parent, Class<T> nodeClass) {
		super(parent, nodeClass, getItems);
	}
}
