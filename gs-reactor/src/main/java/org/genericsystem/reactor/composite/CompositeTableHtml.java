package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.composite.CompositeModel.InputCompositeModel;
import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeModel.StringExtractor;
import org.genericsystem.reactor.flex.FlexColumn;
import org.genericsystem.reactor.flex.FlexRow;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlInputText;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.html.HtmlSection;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class CompositeTableHtml extends FlexColumn<InputCompositeModel> {

	public CompositeTableHtml(HtmlElement<?, ?> parent) {
		this(parent, ObservableListExtractor.ATTRIBUTES);
	}

	public CompositeTableHtml(HtmlElement<?, ?> parent, ObservableListExtractor subObservableListExtractor) {
		super(parent);
		new FlexRow<CompositeModel>(this) {
			{
				addStyle("justify-content", "center");
				addStyle("background-color", "#ffa500");
				addStyle("margin-right", "1px");
				addStyle("margin-bottom", "1px");
				new HtmlH1<CompositeModel>(this) {
					{
						bindStyle("color", "red");
						bindText(CompositeModel::getString);
					}
				};
				// new HtmlButton<CompositeModel>(this) {
				// {
				// setText("red");
				// bindAction(model -> model.getStyleProperty(h1, "color").setValue("red"));
				// }
				// };
				// new HtmlButton<CompositeModel>(this) {
				// {
				// setText("blue");
				// bindAction(model -> model.getStyleProperty(h1, "color").setValue("blue"));
				// }
				// };

			}
		};
		new FlexRow<CompositeModel>(this) {
			{
				new HtmlSection<CompositeModel>(this) {
					{
						addStyle("min-width", "200px");
						addStyle("background-color", "#ffa5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
					}
				};
				new FlexRow<CompositeModel>(this) {
					{
						addStyle("flex", "1");
						addStyle("justify-content", "center");
						addStyle("overflow", "hidden");
						addStyle("color", "#ffffff");
						addStyle("background-color", "#ffa5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> subObservableListExtractor.apply(gs));
						new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
					}
				};
				new FlexRow<CompositeModel>(this) {
					{
						addStyle("flex", "0");
						addStyle("min-width", "100px");
						addStyle("background-color", "#ffa5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
					}
				};
			}
		};
		new FlexRow<CompositeModel>(this) {
			{
				new HtmlSection<CompositeModel>(this) {
					{
						addStyle("min-width", "200px");
						addStyle("background-color", "#dda5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						new HtmlInputText<InputCompositeModel>(this) {
							{
								bindTextBidirectional(InputCompositeModel::getInputString);
								addStyle("width", "100%");
							}
						};
					}
				};
				new FlexRow<CompositeModel>(this) {
					{
						addStyle("flex", "1");
						addStyle("overflow", "hidden");
						addStyle("color", "#ffffff");
						addStyle("background-color", "#dda5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> subObservableListExtractor.apply(gs), InputCompositeModel::new);
						new HtmlInputText<InputCompositeModel>(this) {
							{
								bindTextBidirectional(InputCompositeModel::getInputString);
								addStyle("width", "100%");
							}
						};

					}
				};
				new FlexRow<CompositeModel>(this) {
					{
						addStyle("flex", "0");
						addStyle("min-width", "100px");
						addStyle("background-color", "#dda5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						new HtmlButton<InputCompositeModel>(this) {
							{
								bindAction(model -> {
									model.getGeneric().addInstance(model.getInputString().getValue());
									model.getInputString().setValue(null);
								});
								setText("Add");
								addStyle("width", "100%");
							}
						};
					}
				};
			}
		};

		new FlexRow<CompositeModel>(this) {
			{
				addStyle("overflow", "hidden");
				forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.INSTANCES, CompositeModel::new);
				new FlexColumn<CompositeModel>(this) {
					{
						addStyle("flex", "0");
						addStyle("justify-content", "center");
						addStyle("min-width", "200px");
						addStyle("background-color", "#bba5ff");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						bindText(CompositeModel::getString);
						new HtmlLabel<CompositeModel>(this);
					}
				};
				new FlexColumn<CompositeModel>(this) {
					{
						addStyle("flex", "1");
						addStyle("background-color", "#dda5e2");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> subObservableListExtractor.apply(gs));
						new FlexRow<CompositeModel>(this) {
							{
								addStyle("flex", "1");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.HOLDERS);
								new FlexColumn<CompositeModel>(this) {
									{
										addStyle("justify-content", "center");
										new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
									}
								};
							}
						};
					}
				};
				new FlexRow<CompositeModel>(this) {
					{
						addStyle("flex", "0");
						addStyle("min-width", "100px");
						addStyle("background-color", "#dda5e2");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						addStyle("flex-direction", "column");
						addStyle("justify-content", "center");
						new HtmlButton<CompositeModel>(this) {
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
}
