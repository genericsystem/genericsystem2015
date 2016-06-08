package org.genericsystem.reactor.flex;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.composite.CompositeModel;
import org.genericsystem.reactor.composite.CompositeModel.InputCompositeModel;
import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeModel.StringExtractor;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlInputText;
import org.genericsystem.reactor.html.HtmlLabel;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class FlexTable extends FlexElement<InputCompositeModel> {

	public FlexTable(HtmlElement<?, ?> parent) {
		this(parent, FlexTag.SECTION);
	}

	public FlexTable(HtmlElement<?, ?> parent, FlexTag tag) {
		super(parent, tag, FlexDirection.COLUMN);
		new FlexElement<CompositeModel>(this, FlexTag.HEADER) {
			{
				addStyle("background-color", "#ffa500");
				addStyle("margin-right", "1px");
				addStyle("margin-bottom", "1px");
				addStyle("color", "red");
				addStyle("justify-content", "center");
				new HtmlH1<CompositeModel>(this) {
					{
						bindText(CompositeModel::getString);
					}
				};
			}
		};
		new FlexElement<CompositeModel>(this, FlexTag.SECTION) {
			{
				new FlexElement<CompositeModel>(this, FlexTag.HEADER) {
					{
						addStyle("flex", "0");
						addStyle("color", "#ffffff");
						addStyle("min-width", "200px");
						addStyle("background-color", "#ffa5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						addStyle("justify-content", "center");
						new HtmlLabel<CompositeModel>(this) {
							{
								bindText(CompositeModel::getString);
							}
						};

					};
				};

				new FlexElement<CompositeModel>(this, FlexTag.SECTION) {
					{
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_TYPE);
						addStyle("flex", "1");
						addStyle("overflow", "hidden");
						new FlexElement<InputCompositeModel>(this, FlexTag.SECTION) {
							{
								addStyle("flex", "1");
								addStyle("color", "#ffffff");
								addStyle("background-color", "#ffa5a5");
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								addStyle("justify-content", "center");
								select_(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
								new HtmlLabel<CompositeModel>(this) {
									{
										bindText(CompositeModel::getString);
									}
								};
							}
						};
						new FlexElement<CompositeModel>(this, FlexTag.SECTION) {
							{
								addStyle("flex", "1");
								addStyle("color", "#ffffff");
								addStyle("background-color", "#ffa5a5");
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								addStyle("justify-content", "center");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR,
										gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1])));
								new HtmlLabel<CompositeModel>(this) {
									{
										bindText(CompositeModel::getString);
									}
								};
							}
						};

					}
				};
				new FlexElement<CompositeModel>(this, FlexTag.FOOTER) {
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
		new FlexElement<CompositeModel>(this, FlexTag.SECTION) {
			{
				new FlexElement<CompositeModel>(this, FlexTag.HEADER, FlexDirection.COLUMN) {
					{
						addStyle("flex", "0");
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
				new FlexElement<CompositeModel>(this, FlexTag.SECTION) {
					{
						addStyle("flex", "1");

						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_TYPE);

						new FlexElement<InputCompositeModel>(this, FlexTag.SECTION, FlexDirection.COLUMN) {
							{
								addStyle("flex", "1");
								addStyle("color", "#ffffff");
								addStyle("background-color", "#dda5a5");
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								select(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null, InputCompositeModel::new);
								new HtmlInputText<InputCompositeModel>(this) {
									{
										bindTextBidirectional(InputCompositeModel::getInputString);
										addStyle("width", "100%");
									}
								};
							}
						};
						new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.COLUMN) {
							{
								addStyle("flex", "1");
								addStyle("color", "#ffffff");
								addStyle("background-color", "#dda5a5");
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR,
										gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1])), InputCompositeModel::new);
								new HtmlInputText<InputCompositeModel>(this) {
									{
										bindTextBidirectional(InputCompositeModel::getInputString);
										addStyle("width", "100%");
									}
								};
							}
						};
					}
				};
				new FlexElement<CompositeModel>(this, FlexTag.FOOTER) {
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

		new FlexElement<CompositeModel>(this, FlexTag.SECTION) {
			{
				forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.SUBINSTANCES, CompositeModel::new);
				new FlexElement<CompositeModel>(this, FlexTag.HEADER, FlexDirection.COLUMN) {
					{
						addStyle("flex", "0");
						addStyle("min-width", "200px");
						addStyle("background-color", "#bba5ff");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
					}
				};
				new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.COLUMN) {
					{
						addStyle("flex", "1");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
						new FlexElement<CompositeModel>(this, FlexTag.SECTION) {
							{
								addStyle("flex", "1");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.HOLDERS);
								new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.COLUMN) {
									{
										addStyle("flex", "1");
										addStyle("background-color", "#dda5e2");
										addStyle("margin-right", "1px");
										addStyle("margin-bottom", "1px");
										select_(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
										new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
									}
								};
								new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.COLUMN) {
									{
										addStyle("flex", "1");
										addStyle("background-color", "#dda5e2");
										addStyle("margin-right", "1px");
										addStyle("margin-bottom", "1px");
										forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR,
												gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
										new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
									}
								};
							}
						};
					}
				};
				new FlexElement<CompositeModel>(this, FlexTag.FOOTER, FlexDirection.COLUMN) {
					{
						addStyle("flex", "0");
						addStyle("min-width", "100px");
						addStyle("background-color", "#dda5e2");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						new HtmlButton<CompositeModel>(this) {
							{
								setText("Remove");
								bindAction(CompositeModel::remove);
								addStyle("width", "100%");
								addStyle("height", "100%");
							}
						};
					}
				};
			}
		};
	}
}
