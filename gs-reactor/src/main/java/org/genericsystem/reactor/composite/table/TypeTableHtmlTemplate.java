package org.genericsystem.reactor.composite.table;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.composite.CompositeModel;
import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeModel.StringExtractor;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlInputText;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.html.HtmlSection;

public abstract class TypeTableHtmlTemplate<M extends CompositeModel> extends HtmlSection<M> {

	private ObservableListExtractor subObservableListExtractor = ObservableListExtractor.ATTRIBUTES;

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
				addStyle("margin-right", "1px");
				addStyle("margin-bottom", "1px");
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
						addStyle("background-color", "#ffa5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
					}
				};
				new HtmlSection<CompositeModel>(this) {
					{
						addStyle("display", "flex");
						addStyle("flex-wrap", "nowrap");
						addStyle("flex", "1");
						addStyle("flex-direction", "row");
						addStyle("justify-content", "center");
						addStyle("overflow", "hidden");
						addStyle("color", "#ffffff");
						addStyle("background-color", "#ffa5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> getSubObservableListExtractor().apply(gs));
						new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
					}
				};
				new HtmlSection<M>(this) {
					{
						addStyle("display", "flex");
						addStyle("flex-wrap", "nowrap");
						addStyle("flex", "0");
						addStyle("min-width", "100px");
						addStyle("background-color", "#ffa5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
					}
				};
			}
		};
		new HtmlSection<CompositeModel>(this) {
			{
				addStyle("display", "flex");
				addStyle("flex-wrap", "nowrap");
				addStyle("flex-direction", "row");
				// bindStyle("flex-direction", CompositeModel::getFlexDirection);

				new HtmlSection<M>(this) {
					{
						addStyle("min-width", "200px");
						addStyle("background-color", "#dda5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
					}
				};
				new HtmlSection<CompositeModel>(this) {
					{
						addStyle("display", "flex");
						addStyle("flex-wrap", "nowrap");
						addStyle("flex", "1");
						addStyle("flex-direction", "row");
						// addStyle("justify-content", "center");
						addStyle("overflow", "hidden");
						addStyle("color", "#ffffff");
						addStyle("background-color", "#dda5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> getSubObservableListExtractor().apply(gs));
						new HtmlInputText<CompositeModel>(this) {
							{
								bindText(CompositeModel::getString);
								addStyle("width", "100%");
							}
						};

					}
				};
				new HtmlSection<CompositeModel>(this) {
					{
						addStyle("display", "flex");
						addStyle("flex-wrap", "nowrap");
						addStyle("flex", "0");
						addStyle("min-width", "100px");
						addStyle("background-color", "#dda5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						new HtmlButton<CompositeModel>(this) {
							{
								setText("Add");
								addStyle("width", "100%");
							}
						};
					}
				};
			}
		};

		new HtmlSection<CompositeModel>(this) {
			{
				addStyle("display", "flex");
				addStyle("flex-wrap", "nowrap");
				addStyle("flex-direction", "row");
				addStyle("overflow", "hidden");
				forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.INSTANCES, CompositeModel::new);
				new HtmlSection<M>(this) {
					{
						addStyle("display", "flex");
						addStyle("flex-wrap", "nowrap");
						addStyle("flex", "0");
						addStyle("flex-direction", "column");
						addStyle("justify-content", "center");
						addStyle("min-width", "200px");
						addStyle("background-color", "#bba5ff");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
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
						addStyle("background-color", "#dda5e2");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> getSubObservableListExtractor().apply(gs));
						new HtmlSection<CompositeModel>(this) {
							{
								addStyle("display", "flex");
								addStyle("flex", "1");
								addStyle("flex-direction", "row");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.HOLDERS);
								new HtmlSection<CompositeModel>(this) {
									{
										addStyle("display", "flex");
										addStyle("flex-direction", "column");
										addStyle("justify-content", "center");
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
						addStyle("background-color", "#dda5e2");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						addStyle("flex-direction", "column");
						addStyle("justify-content", "center");
						new HtmlButton<M>(this) {
							{
								setText("Remove");
								bindAction(CompositeModel::remove);
								addStyle("width", "100%");
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
