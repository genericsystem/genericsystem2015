//package org.genericsystem.reactor.composite.table;
//
//import org.genericsystem.reactor.HtmlElement;
//import org.genericsystem.reactor.composite.CompositeModel;
//import org.genericsystem.reactor.composite.CompositeSectionHtmlTemplate;
//import org.genericsystem.reactor.html.HtmlLabel;
//
//public abstract class InstanceAttributeCellHtmlTemplate<M extends CompositeModel, COMPONENT extends InstanceAttributeCellHtmlTemplate<M, COMPONENT>> extends CompositeSectionHtmlTemplate<M, COMPONENT> {
//
//	public InstanceAttributeCellHtmlTemplate(HtmlElement<?, ?, ?> parent) {
//		super(parent);
//	}
//
//	@Override
//	protected void initSubChildren(HtmlSection<CompositeModel> parentSection) {
//		new HtmlSection<CompositeModel>(parentSection) {
//			@Override
//			protected void initChildren() {
//				new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
//			}
//		}.addStyle("flex-direction", "row");
//	}
//
//	public static class InstanceAttributeCellHtml<M extends CompositeModel> extends InstanceAttributeCellHtmlTemplate<M, InstanceAttributeCellHtml<M>> {
//		public InstanceAttributeCellHtml(HtmlElement<?, ?, ?> parent) {
//			super(parent);
//		}
//	}
// }
