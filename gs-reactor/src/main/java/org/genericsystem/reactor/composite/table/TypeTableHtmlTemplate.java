package org.genericsystem.reactor.composite.table;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.composite.CompositeModel;
import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeModel.StringExtractor;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.html.HtmlSection;

public abstract class TypeTableHtmlTemplate<M extends CompositeModel> extends HtmlSection<M> {

	private ObservableListExtractor subObservableListExtractor = ObservableListExtractor.ATTRIBUTES;
	private ObservableListExtractor subSubObservableListExtractor = ObservableListExtractor.HOLDERS;

	public TypeTableHtmlTemplate(HtmlElement<?, ?> parent) {
		super(parent);
		addStyle("display", "flex");
		addStyle("flex-wrap", "nowrap");
		addStyle("flex-direction", "column");
		new HtmlSection<CompositeModel>(this) {
			{
				addStyle("display", "flex");
				addStyle("flex-directionflex", "row");
				addStyle("flex-wrap", "nowrap");
				addStyle("justify-content", "center");
				addStyle("background-color", "#ffa500");
				new HtmlH1<M>(this) {
					{
						bindText(CompositeModel::getString);
					}
				};
			}
		};
		new HtmlSection<CompositeModel>(this) {
			{
				addStyle("display", "flex");
				addStyle("flex-wrap", "nowrap");
				addStyle("flex-direction", "row");
				new HtmlSection<M>(this) {
					{
						addStyle("min-width", "200px");
					}
				};
				new HtmlSection<CompositeModel>(this) {
					{
						addStyle("display", "flex");
						addStyle("flex-wrap", "nowrap");
						addStyle("flex", "1");
						addStyle("justify-content", "center");
						addStyle("background-color", "#538255");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> getSubObservableListExtractor().apply(gs), CompositeModel::new);
						new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
					}
				};
				new HtmlSection<M>(this) {
					{
						addStyle("display", "flex");
						addStyle("flex-wrap", "nowrap");
						addStyle("flex", "0");
						addStyle("min-width", "100px");
					}
				};
			}
		};
		new HtmlSection<CompositeModel>(this) {
			{
				addStyle("display", "flex");
				addStyle("flex-wrap", "nowrap");
				addStyle("flex-direction", "row");
				forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.INSTANCES, CompositeModel::new);
				new HtmlSection<M>(this) {
					{
						addStyle("display", "flex");
						addStyle("flex-wrap", "nowrap");
						addStyle("flex", "0");
						addStyle("min-width", "200px");
						bindText(CompositeModel::getString);
						new HtmlLabel<M>(this);
					}
				};
				new HtmlSection<CompositeModel>(this) {
					{
						addStyle("display", "flex");
						addStyle("flex-wrap", "nowrap");
						addStyle("flex-direction", "column");
						addStyle("flex", "1");
						addStyle("padding", "3");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> getSubObservableListExtractor().apply(gs), CompositeModel::new);
						new HtmlSection<CompositeModel>(this) {
							{
								addStyle("flex", "1");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.HOLDERS, CompositeModel::new);
								new HtmlSection<CompositeModel>(this) {
									{
										addStyle("flex-direction", "row");
										new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
									}
								};
							}
						};
					}
				};
				new HtmlSection<M>(this) {
					{
						addStyle("display", "flex");
						addStyle("flex-wrap", "nowrap");
						addStyle("flex", "0");
						addStyle("min-width", "100px");
						new HtmlButton<M>(this).bindAction(CompositeModel::remove).setText("Remove");
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
