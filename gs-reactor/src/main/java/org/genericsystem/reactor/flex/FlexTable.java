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
		super(parent, tag);
		new FlexElement<CompositeModel>(this, FlexTag.HEADER, FlexDirection.ROW) {
			{
				addStyle("justify-content", "center");
				addStyle("background-color", "#ffa500");
				addStyle("margin-right", "1px");
				addStyle("margin-bottom", "1px");
				addStyle("color", "red");
				new HtmlH1<CompositeModel>(this) {
					{
						bindText(CompositeModel::getString);
					}
				};
			}
		};
		new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
			{
				new FlexElement<CompositeModel>(this, FlexTag.HEADER, FlexDirection.ROW) {
					{
						addStyle("flex", "0");
						addStyle("justify-content", "center");
						addStyle("overflow", "hidden");
						addStyle("color", "#ffffff");
						addStyle("min-width", "200px");
						addStyle("background-color", "#ffa5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
							{
								new HtmlLabel<CompositeModel>(this) {
									{
										bindText(CompositeModel::getString);
										addStyle("width", "100%");
									}
								};
							}
						};
					}
				};
				new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
					{
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_TYPE);
						addStyle("flex", "1");
						new FlexElement<InputCompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
							{
								addStyle("flex", "1");
								addStyle("justify-content", "center");
								addStyle("color", "#ffffff");
								addStyle("background-color", "#ffa5a5");
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								addStyle("overflow", "hidden");
								select_(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
								new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
									{
										new HtmlLabel<CompositeModel>(this) {
											{
												bindText(CompositeModel::getString);
												addStyle("width", "100%");
											}
										};
									}
								};
							}
						};
						new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
							{
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR,
										gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1])));
								addStyle("flex", "1");
								addStyle("justify-content", "center");
								addStyle("color", "#ffffff");
								addStyle("background-color", "#ffa5a5");
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								addStyle("overflow", "hidden");
								new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
									{
										new HtmlLabel<CompositeModel>(this) {
											{
												bindText(CompositeModel::getString);
												addStyle("width", "100%");
											}
										};
									}
								};
							}
						};

					}
				};
				new FlexElement<CompositeModel>(this, FlexTag.FOOTER, FlexDirection.ROW) {
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
		new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
			{
				new FlexElement<CompositeModel>(this, FlexTag.HEADER) {
					{
						addStyle("flex", "0");
						addStyle("min-width", "200px");
						addStyle("background-color", "#dda5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						new FlexElement<InputCompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
							{
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
				new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
					{
						addStyle("flex", "1");

						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_TYPE);

						new FlexElement<InputCompositeModel>(this, FlexTag.SECTION) {
							{
								addStyle("flex", "1");
								addStyle("color", "#ffffff");
								addStyle("background-color", "#dda5a5");
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								select(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null, InputCompositeModel::new);
								new FlexElement<InputCompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
									{
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
						new FlexElement<CompositeModel>(this, FlexTag.SECTION) {
							{
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR,
										gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1])), InputCompositeModel::new);
								addStyle("flex", "1");
								addStyle("color", "#ffffff");
								addStyle("background-color", "#dda5a5");
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								new FlexElement<InputCompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
									{
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

						// new HtmlInputText<InputCompositeModel>(this) {
						// {
						// bindTextBidirectional(InputCompositeModel::getInputString);
						// addStyle("width", "100%");
						// }
						// };
					}
				};
				new FlexElement<CompositeModel>(this, FlexTag.FOOTER, FlexDirection.ROW) {
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

		new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
			{
				addStyle("overflow", "hidden");
				forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.SUBINSTANCES, CompositeModel::new);
				new FlexElement<CompositeModel>(this, FlexTag.HEADER) {
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
				new FlexElement<CompositeModel>(this, FlexTag.SECTION) {
					{
						addStyle("flex", "1");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
						new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
							{
								addStyle("flex", "1");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.HOLDERS);
								new FlexElement<CompositeModel>(this, FlexTag.SECTION) {
									{
										addStyle("flex", "1");
										addStyle("justify-content", "center");
										addStyle("background-color", "#dda5e2");
										addStyle("margin-right", "1px");
										addStyle("margin-bottom", "1px");
										select_(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
										new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
											{
												new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
											}
										};
									}
								};
								new FlexElement<CompositeModel>(this, FlexTag.SECTION) {
									{
										forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR,
												gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
										addStyle("flex", "1");
										addStyle("justify-content", "center");
										addStyle("background-color", "#dda5e2");
										addStyle("margin-right", "1px");
										addStyle("margin-bottom", "1px");
										new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
											{
												new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
											}
										};
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
								addStyle("height", "100%");
							}
						};
					}
				};
			}
		};
	}
}
