package org.genericsystem.gui.javafx;

import javafx.scene.control.TableColumn;

import org.genericsystem.gui.context.RootContext;

public class GSTableColum<Generic, String> extends TableColumn<Generic, String> {
	private RootContext rootContext;

	public GSTableColum(RootContext rootContext) {
		super(rootContext.rootProperty.getValue().toString());
		this.rootContext = rootContext;
	}

}
