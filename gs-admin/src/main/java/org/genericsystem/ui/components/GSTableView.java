package org.genericsystem.ui.components;

import javafx.scene.control.TableView;

import org.genericsystem.todoKernel.Element;

public class GSTableView extends GSRegion<GSTableView, TableView> {

	public GSTableView(Element<?> parent) {
		super(parent, TableView.class);
	}

}
