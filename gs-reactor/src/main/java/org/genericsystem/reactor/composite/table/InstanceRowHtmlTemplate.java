//package org.genericsystem.reactor.composite.table;
//
//import org.genericsystem.reactor.composite.CompositeModel;
//import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
//import org.genericsystem.reactor.composite.CompositeSectionHtmlTemplate;
//import org.genericsystem.reactor.html.HtmlButton;
//import org.genericsystem.reactor.html.HtmlLabel;
//
//public abstract class InstanceRowHtmlTemplate<M extends CompositeModel, COMPONENT extends InstanceRowHtmlTemplate<M, COMPONENT>> extends CompositeSectionHtmlTemplate<M, COMPONENT> {
//
//	public InstanceRowHtmlTemplate(HtmlSection<CompositeModel> parent) {
//		super(parent);
//	}
//
//	@Override
//	protected void initChildren() {
//		new HtmlLabel<M>(new HtmlSection<>(this).addStyle("min-width", "200px")).bindText(CompositeModel::getString);
//		super.initChildren();
//		new HtmlButton<M>(new HtmlSection<>(this).addStyle("min-width", "80px")).bindAction(CompositeModel::remove).setText("Remove");
//	}
//
//	@Override
//	protected void initSubChildren(HtmlSection<CompositeModel> parentSection) {
//		new CompositeSectionHtml<CompositeModel>(parentSection) {
//			@Override
//			protected void initSubChildren(HtmlSection<CompositeModel> parentSection) {
//				new HtmlSection<CompositeModel>(parentSection) {
//					@Override
//					protected void initChildren() {
//						new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
//					}
//				}.addStyle("flex-direction", "row");
//			}
//		}.addStyle("flex-direction", "column").setObservableListExtractor(ObservableListExtractor.HOLDERS);
//	}
//
//	public static class InstanceRowHtml<M extends CompositeModel> extends InstanceRowHtmlTemplate<M, InstanceRowHtml<M>> {
//		public InstanceRowHtml(HtmlSection<CompositeModel> parent) {
//			super(parent);
//		}
//	}
// }
