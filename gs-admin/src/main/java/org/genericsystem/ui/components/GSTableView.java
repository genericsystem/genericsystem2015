package org.genericsystem.ui.components;

import java.util.function.Function;

import javafx.collections.ObservableList;
import javafx.scene.control.TableView;

import org.genericsystem.ui.Element;

public class GSTableView extends GSRegion<GSTableView, TableView> {

	public GSTableView(Element<?> parent) {
		super(parent, TableView.class);
	}

	public <M, T> GSTableView setItemsObservableList(Function<M, ObservableList<T>> function) {
		setObservableList(TableView::itemsProperty, function);
		return this;
	}

}
