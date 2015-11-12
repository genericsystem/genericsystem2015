package org.genericsystem.gui.javafx;

import javafx.scene.control.TableRow;

import org.genericsystem.common.Generic;
import org.genericsystem.gui.context.SubContext;

public class GSTableRow extends TableRow<Generic> {

	public SubContext subContext;

	public GSTableRow(SubContext sub) {
		this.subContext = sub;
	}

	@Override
	public void commitEdit(Generic newValue) {
		super.commitEdit(newValue);
	}

	@Override
	public void cancelEdit() {
		super.cancelEdit();
	}
}