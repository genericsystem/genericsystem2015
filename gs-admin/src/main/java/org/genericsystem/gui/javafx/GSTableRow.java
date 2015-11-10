package org.genericsystem.gui.javafx;

import javafx.scene.control.TableRow;

import org.genericsystem.gui.context.SubContext;

public class GSTableRow<Generic> extends TableRow<Generic> {

	public SubContext subContext;

	public GSTableRow(SubContext subContext) {

		this.subContext = subContext;
		this.setItem((Generic) subContext.observableGeneric.getValue());
	}

	@Override
	public void commitEdit(Generic newValue) {
		// TODO Auto-generated method stub
		super.commitEdit(newValue);
	}

	@Override
	public void cancelEdit() {
		// TODO Auto-generated method stub
		super.cancelEdit();
	}
}
