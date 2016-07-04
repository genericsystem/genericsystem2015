package org.genericsystem.reactor.flex;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.Visitor.CheckInputsValidityVisitor;
import org.genericsystem.reactor.Visitor.ClearVisitor;
import org.genericsystem.reactor.Visitor.HolderVisitor;
import org.genericsystem.reactor.annotation.InstanceColorize;
import org.genericsystem.reactor.composite.CompositeSelect.CompositeSelectWithEmptyEntry;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlHyperLink;
import org.genericsystem.reactor.html.HtmlInputText;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.GenericModel.StringExtractor;
import org.genericsystem.reactor.model.InputGenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.SelectorModel;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class FlexTable extends CompositeFlexElement<InputGenericModel> {

	public FlexTable(Tag<?> parent) {
		super(parent, FlexDirection.COLUMN);
	}

	@Override
	protected void header() {
		titleHeader();
		columnsTitleSection();
		columnsInputSection();
	}

	protected void titleHeader() {
		new FlexTag<GenericModel>(this, FlexDirection.ROW) {
			{
				addStyle("background-color", "#ffa500");
				addStyle("margin-right", "1px");
				addStyle("margin-bottom", "1px");
				addStyle("color", "red");
				addStyle("justify-content", "center");
				new HtmlH1<GenericModel>(this) {
					{
						bindText(GenericModel::getString);
					}
				};
			}
		};
	}

	protected void columnsTitleSection() {
		new CompositeFlexElement<GenericModel>(this, FlexDirection.ROW) {

			@Override
			protected void header() {
				new FlexTag<GenericModel>(this, FlexDirection.ROW) {
					{
						addStyle("flex", "0");
						addStyle("color", "#ffffff");
						addStyle("min-width", "200px");
						addStyle("background-color", "#ffa5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						addStyle("justify-content", "center");
						new HtmlLabel<GenericModel>(this) {
							{
								bindText(GenericModel::getString);
							}
						};

					};
				};
			}

			@Override
			protected void sections() {
				new CompositeFlexElement<GenericModel>(this, FlexDirection.ROW) {
					{
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_TYPE);
						addStyle("flex", "1");
						addStyle("overflow", "hidden");
					}

					@Override
					protected void header() {
						new FlexTag<InputGenericModel>(this, FlexDirection.ROW) {
							{
								addStyle("flex", "1");
								addStyle("color", "#ffffff");
								addStyle("background-color", "#ffa5a5");
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								addStyle("justify-content", "center");
								select_(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
								new HtmlLabel<GenericModel>(this) {
									{
										bindText(GenericModel::getString);
									}
								};
							}
						};
					}

					@Override
					protected void sections() {
						new FlexTag<GenericModel>(this, FlexDirection.ROW) {
							{
								addStyle("flex", "1");
								addStyle("color", "#ffffff");
								addStyle("background-color", "#ffa5a5");
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								addStyle("justify-content", "center");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR,
										gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1])));
								new HtmlLabel<GenericModel>(this) {
									{
										bindText(GenericModel::getString);
									}
								};
							}
						};
					}
				};
			}

			@Override
			protected void footer() {
				new FlexTag<GenericModel>(this, FlexDirection.ROW) {
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
		new CompositeFlexElement<GenericModel>(this, FlexDirection.ROW) {

			@Override
			protected void header() {
				new FlexTag<GenericModel>(this, FlexDirection.COLUMN) {
					{
						addStyle("flex", "0");
						addStyle("min-width", "200px");
						addStyle("background-color", "#dda5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						new HtmlInputText<InputGenericModel>(this) {
							{
								bindOptionalStyle("background-color", InputGenericModel::getInvalid, "yellow");
								bindOperation((gs, value, g) -> gs[0].setInstance(value));
								bindTextBidirectional(InputGenericModel::getInputString);
							}
						};

					}
				};
			}

			@Override
			protected void sections() {
				new FlexTag<GenericModel>(this, FlexDirection.ROW) {
					{
						addStyle("flex", "1");
						addStyle("overflow", "hidden");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_TYPE);
						new FlexTag<InputGenericModel>(this, FlexDirection.COLUMN) {
							{
								addStyle("flex", "1");
								addStyle("color", "#ffffff");
								addStyle("background-color", "#dda5a5");
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								addStyle("overflow", "hidden");
								select(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null, InputGenericModel::new);
								new HtmlInputText<InputGenericModel>(this) {
									{
										bindOptionalStyle("border-color", InputGenericModel::getInvalid, "red");
										bindOperation((gs, value, g) -> g.setHolder(gs[1], value));
										bindTextBidirectional(InputGenericModel::getInputString);
									}
								};
							}
						};
						new FlexTag<GenericModel>(this, FlexDirection.COLUMN) {
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
				new FlexTag<GenericModel>(this, FlexDirection.ROW) {
					{
						addStyle("flex", "0");
						addStyle("min-width", "100px");
						addStyle("background-color", "#dda5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						new HtmlButton<InputGenericModel>(this) {
							{
								bindOptionalAttribute("disabled", model -> new CheckInputsValidityVisitor(model.getModelContext()).isInvalid(), "disabled");
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
		new CompositeFlexElement<GenericModel>(this, FlexDirection.ROW) {
			{
				forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.SUBINSTANCES, GenericModel::new);
			}

			@Override
			protected void header() {
				new FlexTag<GenericModel>(this, FlexDirection.COLUMN) {
					{
						addStyle("flex", "0");
						addStyle("min-width", "200px");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						addStyle("overflow", "hidden");
						addPrefixBinding(modelContext -> modelContext.getObservableStyles(this).put("background-color",
								modelContext.<GenericModel> getModel().getGeneric().getMeta().getAnnotation(InstanceColorize.class) != null
										? modelContext.<GenericModel> getModel().getString().getValue() : "#bba5ff"));
						new HtmlHyperLink<GenericModel>(this) {
							{
								bindText(GenericModel::getString);
								bindAction(compositeModel -> {
									GenericModel cm = compositeModel;
									while (!(cm instanceof SelectorModel) && cm != null)
										cm = (GenericModel) cm.getParent();
									((SelectorModel) cm).getSelection().setValue(compositeModel);
								});
							}
						};

					}
				};
			}

			@Override
			protected void sections() {
				new FlexTag<GenericModel>(this, FlexDirection.COLUMN) {
					{
						addStyle("flex", "1");
						addStyle("overflow", "hidden");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
						new FlexTag<GenericModel>(this, FlexDirection.ROW) {
							{
								addStyle("flex", "1");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.HOLDERS);
								new FlexTag<GenericModel>(this, FlexDirection.COLUMN) {
									{
										addStyle("flex", "1");
										addStyle("background-color", "#dda5e2");
										addStyle("margin-right", "1px");
										addStyle("margin-bottom", "1px");
										addStyle("overflow", "hidden");
										select_(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
										new HtmlLabel<GenericModel>(this).bindText(GenericModel::getString);
									}
								};
								new FlexTag<GenericModel>(this, FlexDirection.COLUMN) {
									{
										addStyle("overflow", "hidden");
										addStyle("flex", "1");
										addPrefixBinding(
												modelContext -> modelContext.getObservableStyles(this).put("background-color",
														modelContext.<GenericModel> getModel().getGeneric().getMeta()
																.getAnnotation(InstanceColorize.class) != null
																		? modelContext.<GenericModel> getModel().getString().getValue() : "#dda5e2"));
										addStyle("margin-right", "1px");
										addStyle("margin-bottom", "1px");
										forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR,
												gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
										new HtmlLabel<GenericModel>(this).bindText(GenericModel::getString);
									}
								};
							}
						};
					}
				};
			}

			@Override
			protected void footer() {
				new FlexTag<GenericModel>(this, FlexDirection.COLUMN) {
					{
						addStyle("flex", "0");
						addStyle("min-width", "100px");
						addStyle("background-color", "#dda5e2");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						new HtmlButton<GenericModel>(this) {
							{
								setText("Remove");
								bindAction(GenericModel::remove);
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
