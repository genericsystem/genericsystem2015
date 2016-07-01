package org.genericsystem.reactor.flex;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.Visitor.ClearVisitor;
import org.genericsystem.reactor.Visitor.HolderVisitor;
import org.genericsystem.reactor.annotation.InstanceColorize;
import org.genericsystem.reactor.composite.CompositeSelect.CompositeSelectWithEmptyEntry;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlHyperLink;
import org.genericsystem.reactor.html.HtmlInputText;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.model.CompositeModel;
import org.genericsystem.reactor.model.CompositeModel.StringExtractor;
import org.genericsystem.reactor.model.InputCompositeModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.SelectorModel;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class FlexTable extends CompositeFlexElement<InputCompositeModel> {

	public FlexTable(Element<?> parent) {
		this(parent, FlexTag.SECTION);
	}

	public FlexTable(Element<?> parent, FlexTag tag) {
		super(parent, tag, FlexDirection.COLUMN);
	}

	@Override
	protected void header() {
		titleHeader();
		columnsTitleSection();
		columnsInputSection();
	}

	protected void titleHeader() {
		new FlexElement<CompositeModel>(this, FlexTag.HEADER, FlexDirection.ROW) {
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
	}

	protected void columnsTitleSection() {
		new CompositeFlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {

			@Override
			protected void header() {
				new FlexElement<CompositeModel>(this, FlexTag.HEADER, FlexDirection.ROW) {
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
			}

			@Override
			protected void sections() {
				new CompositeFlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
					{
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_TYPE);
						addStyle("flex", "1");
						addStyle("overflow", "hidden");
					}

					@Override
					protected void header() {
						new FlexElement<InputCompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
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
					}

					@Override
					protected void sections() {
						new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
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
			}

			@Override
			protected void footer() {
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
	}

	protected void columnsInputSection() {
		new CompositeFlexElement<CompositeModel>(this, FlexTag.FOOTER, FlexDirection.ROW) {

			@Override
			protected void header() {
				new FlexElement<CompositeModel>(this, FlexTag.HEADER, FlexDirection.COLUMN) {
					{
						addStyle("flex", "0");
						addStyle("min-width", "200px");
						addStyle("background-color", "#dda5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						new HtmlInputText<InputCompositeModel>(this) {
							{
								bindOptionalStyle("background-color", InputCompositeModel::getInvalid, "yellow");
								bindOperation((gs, value, g) -> gs[0].setInstance(value));
								bindTextBidirectional(InputCompositeModel::getInputString);
							}
						};

					}
				};
			}

			@Override
			protected void sections() {
				new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
					{
						addStyle("flex", "1");
						addStyle("overflow", "hidden");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_TYPE);
						new FlexElement<InputCompositeModel>(this, FlexTag.SECTION, FlexDirection.COLUMN) {
							{
								addStyle("flex", "1");
								addStyle("color", "#ffffff");
								addStyle("background-color", "#dda5a5");
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								addStyle("overflow", "hidden");
								select(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null, InputCompositeModel::new);
								new HtmlInputText<InputCompositeModel>(this) {
									{
										bindOptionalStyle("border-color", InputCompositeModel::getInvalid, "red");
										bindOperation((gs, value, g) -> g.setHolder(gs[1], value));
										bindTextBidirectional(InputCompositeModel::getInputString);
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
								addStyle("overflow", "hidden");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR,
										gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1])), SelectorModel::new);
								new CompositeSelectWithEmptyEntry<SelectorModel>(this) {
									{
										// addStyle("width", "100%");
									}
								};
							}
						};
					}
				};
			};

			@Override
			protected void footer() {
				new FlexElement<CompositeModel>(this, FlexTag.FOOTER, FlexDirection.ROW) {
					{
						addStyle("flex", "0");
						addStyle("min-width", "100px");
						addStyle("background-color", "#dda5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						new HtmlButton<InputCompositeModel>(this) {
							{
								bindAction2(modelContext -> {
									try {
										new HolderVisitor().visit(modelContext);
										new ClearVisitor().visit(modelContext);
									} catch (RollbackException e) {
										e.printStackTrace();
									}
								});
								setText("Add");
								addStyle("width", "100%");
							}
						};
					}
				};
			}
		};
	}

	@Override
	protected void sections() {
		new CompositeFlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
			{
				forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.SUBINSTANCES, CompositeModel::new);
			}

			@Override
			protected void header() {
				new FlexElement<CompositeModel>(this, FlexTag.HEADER, FlexDirection.COLUMN) {
					{
						addStyle("flex", "0");
						addStyle("min-width", "200px");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						addStyle("overflow", "hidden");
						addPrefixBinding(modelContext -> modelContext.getObservableStyles(this).put("background-color",
								modelContext.<CompositeModel> getModel().getGeneric().getMeta().getAnnotation(InstanceColorize.class) != null
										? modelContext.<CompositeModel> getModel().getString().getValue() : "#bba5ff"));
						new HtmlHyperLink<CompositeModel>(this) {
							{
								bindText(CompositeModel::getString);
								bindAction(compositeModel -> {
									CompositeModel cm = compositeModel;
									while (!(cm instanceof SelectorModel) && cm != null)
										cm = (CompositeModel) cm.getParent();
									((SelectorModel) cm).getSelection().setValue(compositeModel);
								});
							}
						};

					}
				};
			}

			@Override
			protected void sections() {
				new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.COLUMN) {
					{
						addStyle("flex", "1");
						addStyle("overflow", "hidden");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
						new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
							{
								addStyle("flex", "1");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.HOLDERS);
								new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.COLUMN) {
									{
										addStyle("flex", "1");
										addStyle("background-color", "#dda5e2");
										addStyle("margin-right", "1px");
										addStyle("margin-bottom", "1px");
										addStyle("overflow", "hidden");
										select_(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
										new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
									}
								};
								new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.COLUMN) {
									{
										addStyle("overflow", "hidden");
										addStyle("flex", "1");
										addPrefixBinding(
												modelContext -> modelContext.getObservableStyles(this).put("background-color",
														modelContext.<CompositeModel> getModel().getGeneric().getMeta()
																.getAnnotation(InstanceColorize.class) != null
																		? modelContext.<CompositeModel> getModel().getString().getValue() : "#dda5e2"));
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
			}

			@Override
			protected void footer() {
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
