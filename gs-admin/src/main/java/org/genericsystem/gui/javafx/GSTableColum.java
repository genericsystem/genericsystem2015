package org.genericsystem.gui.javafx;

import javafx.scene.control.TableColumn;

import org.genericsystem.gui.context.SubContext;

public class GSTableColum<Generic, String> extends TableColumn<Generic, String> {
	private SubContext rootContext;

	public GSTableColum(SubContext subContext) {
		// super(subContext.);
		super("name");
		this.rootContext = rootContext;
	}
}
