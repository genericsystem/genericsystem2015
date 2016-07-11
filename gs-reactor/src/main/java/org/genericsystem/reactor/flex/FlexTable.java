package org.genericsystem.reactor.flex;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.Visitor.CheckInputsValidityVisitor;
import org.genericsystem.reactor.Visitor.ClearVisitor;
import org.genericsystem.reactor.Visitor.HolderVisitor;
import org.genericsystem.reactor.annotation.InstanceColorize;
import org.genericsystem.reactor.composite.CompositeSelect.CompositeSelectWithEmptyEntry;
import org.genericsystem.reactor.flex.FlexLinks.FlexLinkEditor;
import org.genericsystem.reactor.flex.FlexLinks.FlexLinkTitleDisplayer;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlCheckBox;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlHyperLink;
import org.genericsystem.reactor.html.HtmlInputText;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.GenericModel.StringExtractor;
import org.genericsystem.reactor.model.InputCheckModel;
import org.genericsystem.reactor.model.InputGenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.SelectorModel;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class FlexTable extends CompositeFlexSection<GenericModel> {

	public FlexTable(Tag<?> parent) {
		super(parent, FlexDirection.COLUMN);
	}

	public FlexTable(Tag<?> parent, FlexDirection flexDirection) {
		super(parent, flexDirection);
		addStyle("flex", "1");
	}

	@Override
	protected void header() {
		titleHeader();
		columnsTitleSection();
		columnsInputSection();
	}

	protected void titleHeader() {
		new FlexSection<GenericModel>(this, this.getReverseDirection()) {
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
		new CompositeFlexSection<GenericModel>(this, this.getReverseDirection()) {

			@Override
			protected void header() {
				new FlexSection<GenericModel>(this, this.getDirection()) {
					{
						addStyle("flex", "1");
						addStyle("color", "#ffffff");
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
				new FlexLinkTitleDisplayer<GenericModel>(this, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1])), this.getDirection()) {
					{
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_TYPE);
						addStyle("flex", "1");
						addStyle("overflow", "hidden");
					}
				};
			}

			@Override
			protected void footer() {
				new FlexSection<GenericModel>(this, this.getDirection()) {
					{
						if (this.getDirection().equals(FlexDirection.ROW)) {
							addStyle("flex", "0");
							addStyle("min-width", "100px");
						} else {
							addStyle("flex", "1");
						}
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
		new CompositeFlexSection<GenericModel>(this, this.getReverseDirection()) {
			{
				select(gs -> gs[0], InputGenericModel::new);
			}

			@Override
			protected void header() {
				new FlexSection<GenericModel>(this, this.getReverseDirection()) {
					{
						addStyle("flex", "1");
						addStyle("background-color", "#dda5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						new HtmlInputText<InputGenericModel>(this) {
							{
								addStyle("width", "100%");
								addStyle("height", "100%");
								bindOptionalStyle("border-color", InputGenericModel::getInvalid, "red");
								bindAction((gs, value, g) -> gs[0].setInstance(value));
								bindTextBidirectional(InputGenericModel::getInputString);
							}
						};

					}
				};
			}

			@Override
			protected void sections() {
				new FlexSection<GenericModel>(this, this.getDirection()) {
					{
						addStyle("flex", "1");
						addStyle("overflow", "hidden");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_TYPE);
						new FlexSection<GenericModel>(this, this.getReverseDirection()) {
							{
								addStyle("flex", "1");
								addStyle("color", "#ffffff");
								addStyle("background-color", "#dda5a5");
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								addStyle("overflow", "hidden");
								select(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null, GenericModel::new);
								new FlexSection<GenericModel>(this, this.getDirection()) {
									{
										addStyle("justify-content", "center");
										addStyle("align-items", "center");
										addStyle("width", "100%");
										addStyle("height", "100%");
										new HtmlInputText<InputGenericModel>(this) {
											{
												select(gs -> !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null, InputGenericModel::new);
												addStyle("width", "100%");
												addStyle("height", "100%");
												bindOptionalStyle("border-color", InputGenericModel::getInvalid, "red");
												bindTextBidirectional(InputGenericModel::getInputString);
												bindAction((gs, value, g) -> g.setHolder(gs[1], value));
											}
										};
										new HtmlCheckBox<InputCheckModel>(this) {
											{
												select(gs -> Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null, InputCheckModel::new);
												bindOperation((gs, value, g) -> g.setHolder(gs[1], value));
												bindCheckedBidirectional(InputCheckModel::getChecked);
											}
										};
									}
								};
							}
						};
						new FlexSection<GenericModel>(this, this.getReverseDirection()) {
							{
								addStyle("flex", "1");
								addStyle("color", "#ffffff");
								addStyle("background-color", "#dda5a5");
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								addStyle("overflow", "hidden");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1])), SelectorModel::new);
								new CompositeSelectWithEmptyEntry<SelectorModel>(this) {
									{
										addStyle("width", "100%");
										addStyle("height", "100%");
									}
								};
							}
						};
					}
				};
			};

			@Override
			protected void footer() {
				new FlexSection<GenericModel>(this, this.getDirection()) {
					{
						if (this.getDirection().equals(FlexDirection.ROW)) {
							addStyle("flex", "0");
							addStyle("min-width", "100px");
						} else {
							addStyle("flex", "1");
						}
						addStyle("background-color", "#dda5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						new HtmlButton<InputGenericModel>(this) {
							{
								bindOptionalAttribute("disabled", model -> new CheckInputsValidityVisitor(model).isInvalid(), "disabled");
								bindAction(modelContext -> {
									try {
										new HolderVisitor().visit(modelContext);
										new ClearVisitor().visit(modelContext);
									} catch (RollbackException e) {
										e.printStackTrace();
									}
								});
								setText("Add");
								addStyle("width", "100%");
								addStyle("height", "100%");
							}
						};
					}
				};
			}
		};
	}

	@Override
	protected void sections() {
		new CompositeFlexSection<GenericModel>(this, this.getReverseDirection()) {
			{
				addStyle("flex", "1");
				forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.SUBINSTANCES, GenericModel::new);
			}

			@Override
			protected void header() {
				new FlexSection<GenericModel>(this, this.getReverseDirection()) {
					{
						addStyle("flex", "1");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						addStyle("overflow", "hidden");
						addPrefixBinding(modelContext -> modelContext.getObservableStyles(this).put("background-color", modelContext.getGeneric().getMeta().getAnnotation(InstanceColorize.class) != null ? modelContext.getString().getValue() : "#bba5ff"));
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
				new FlexSection<GenericModel>(this, this.getReverseDirection()) {
					{
						addStyle("flex", "1");
						addStyle("overflow", "hidden");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
						new FlexLinkEditor<GenericModel>(this, this.getReverseDirection()) {
							{
								addStyle("flex", "1");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.HOLDERS);
							}
						};
					}
				};
			}

			@Override
			protected void footer() {
				new FlexSection<GenericModel>(this, this.getDirection()) {
					{
						if (this.getDirection().equals(FlexDirection.ROW)) {
							addStyle("flex", "0");
							addStyle("min-width", "100px");
						} else {
							addStyle("flex", "1");
						}
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
