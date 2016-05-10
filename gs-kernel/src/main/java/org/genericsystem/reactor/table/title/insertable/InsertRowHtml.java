package org.genericsystem.reactor.table.title.insertable;
//package org.genericsystem.distributed.cacheonserver.ui.table.title.insertable;
//
//import org.genericsystem.distributed.cacheonserver.ui.table.TypeTableHtml;
//import org.genericsystem.distributed.ui.components.HtmlButton;
//import org.genericsystem.distributed.ui.components.HtmlInputText;
//import org.genericsystem.distributed.ui.components.HtmlSection;
//
//public class InsertRowHtml<M extends InsertRowModel> extends HtmlSection<M> {
//
//	public InsertRowHtml(TypeTableHtml<?> parent) {
//		super(parent);
//		addStyleClass("gsrow gsinsertrow");
//	}
//
//	@Override
//	protected void initChildren() {
//		new HtmlInputText<M>(new HtmlSection<>(this).addStyleClass("gscell").addStyleClass("gstitlecell")).bindTextBidirectional(InsertRowModel::getInputString);
//		new InsertAttributeCellHtml<InsertAttributeCellModel>(this).forEach(InsertRowModel::getSubModels);
//		new HtmlButton<M>(new HtmlSection<>(this).addStyleClass("gscell").addStyleClass("gsbuttoncell")).bindAction(InsertRowModel::create).setText("Create");
//	}
//
// }
