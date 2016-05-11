package org.genericsystem.reactor.composite.table.title;
//package org.genericsystem.distributed.cacheonserver.ui.table.title;
//
//import org.genericsystem.distributed.cacheonserver.ui.table.InstanceRowHtml;
//import org.genericsystem.distributed.cacheonserver.ui.table.TypeTableHtml;
//import org.genericsystem.distributed.ui.HtmlElement;
//import org.genericsystem.distributed.ui.CompositeModel.StringExtractor;
//import org.genericsystem.distributed.ui.components.HtmlH1;
//import org.genericsystem.distributed.ui.components.HtmlSection;
//
//public class TitleTypeTableHtml<M extends TitleTypeTableModel> extends TypeTableHtml<M> {
//	public TitleTypeTableHtml(HtmlElement<?, ?, ?> parent, StringExtractor extractor) {
//		super(parent, extractor);
//	}
//
//	@Override
//	protected void initChildren() {
//		new HtmlH1<M>(new HtmlSection<>(this).addStyleClass("gsrow").addStyleClass("gstitlerow")).bindText(TitleTypeTableModel::getString);
//		new TitleRowHtml<>(this).select(TitleTypeTableModel::getTitleRowModel);
//		new InstanceRowHtml<>(this, StringExtractor.SIMPLE_CLASS_EXTRACTOR).forEach(TitleTypeTableModel::getSubModels);
//	}
//
// }
