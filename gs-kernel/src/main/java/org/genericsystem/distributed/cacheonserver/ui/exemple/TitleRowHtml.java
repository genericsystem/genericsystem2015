package org.genericsystem.distributed.cacheonserver.ui.exemple;

import org.genericsystem.distributed.ui.components.HtmlH1;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class TitleRowHtml extends HtmlSection {

	public TitleRowHtml(TypeTableHtml parent) {
		super(parent);
		setStyleClass("gsrow");
	}

	@Override
	protected void initChildren() {
		new HtmlH1(this).bindText(TitleRowModel::getTitleString);
	}

}
