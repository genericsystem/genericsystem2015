package org.genericsystem.reactor.composite.table;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.composite.CompositeModel;
import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeSectionHtmlTemplate;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.html.HtmlSection;

public abstract class TypeTableHtmlTemplate<M extends CompositeModel> extends CompositeSectionHtmlTemplate<M> {

	private ObservableListExtractor subObservableListExtractor = ObservableListExtractor.ATTRIBUTES;
	private ObservableListExtractor subSubObservableListExtractor = ObservableListExtractor.HOLDERS;

	public TypeTableHtmlTemplate(HtmlElement<?, ?> parent) {
		super(parent);
		addStyle("flex-direction", "column");
		setObservableListExtractor(ObservableListExtractor.INSTANCES);
		new HtmlSection<CompositeModel>(this) {
			{
				new HtmlH1<M>(this);
			}
		};
		new CompositeSectionHtmlTemplate<CompositeModel>(this) {
			{
				addStyle("flex-direction", "row");
				setObservableListExtractor(gs -> getSubObservableListExtractor().apply(gs));
				new HtmlSection<M>(this) {
					{
						addStyle("min-width", "200px");
					}
				};
				new HtmlSection<CompositeModel>(this) {
					{
						addStyle("flex", "1");
						forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (gs, stringExtractor) -> getModelConstructor().build(gs, stringExtractor));
						new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
					}
				};
				new HtmlSection<M>(this) {
					{
						addStyle("min-width", "80px");
					}
				};
			}
		};
		new HtmlSection<CompositeModel>(this) {
			{
				addStyle("flex", "1");
				forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (gs, stringExtractor) -> getModelConstructor().build(gs, stringExtractor));

				new CompositeSectionHtmlTemplate<CompositeModel>(this) {
					{
						addStyle("flex-direction", "row");
						setObservableListExtractor(gs -> getSubObservableListExtractor().apply(gs));
						new HtmlSection<M>(this) {
							{
								addStyle("min-width", "200px");
								bindText(CompositeModel::getString);
								new HtmlLabel<M>(this);
							}
						};
						new HtmlSection<CompositeModel>(this) {
							{
								addStyle("flex", "1");
								forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (gs, stringExtractor) -> getModelConstructor().build(gs, stringExtractor));
								new CompositeSectionHtmlTemplate<CompositeModel>(this) {
									{
										addStyle("flex-direction", "column");
										setObservableListExtractor(gs -> getSubSubObservableListExtractor().apply(gs));
										new HtmlSection<CompositeModel>(this) {
											{
												addStyle("flex", "1");
												forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (gs, stringExtractor) -> getModelConstructor().build(gs, stringExtractor));
												new HtmlSection<CompositeModel>(this) {
													{
														addStyle("flex-direction", "row");
														new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
													}
												};
											}
										};
									};
								};
							}
						};
						new HtmlSection<M>(this) {
							{
								addStyle("min-width", "80px");
								new HtmlButton<M>(this).bindAction(CompositeModel::remove).setText("Remove");
							}
						};
					}
				};
			}
		};

	}

	public ObservableListExtractor getSubObservableListExtractor() {
		return subObservableListExtractor;
	}

	public ObservableListExtractor getSubSubObservableListExtractor() {
		return subSubObservableListExtractor;
	}

	public TypeTableHtmlTemplate<M> setSubObservableListExtractor(ObservableListExtractor subObservableListExtractor) {
		this.subObservableListExtractor = subObservableListExtractor;
		return this;
	}

	public static class TypeTableHtml<M extends CompositeModel> extends TypeTableHtmlTemplate<M> {
		public TypeTableHtml(HtmlElement<?, ?> parent) {
			super(parent);
		}
	}
}
