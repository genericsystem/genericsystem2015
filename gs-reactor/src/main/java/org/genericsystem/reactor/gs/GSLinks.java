package org.genericsystem.reactor.gs;

import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

import java.io.Serializable;
import java.util.List;

import javafx.beans.property.Property;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.Visitor.ClearVisitor;
import org.genericsystem.reactor.Visitor.HolderVisitor;
import org.genericsystem.reactor.gs.GSSelect.ColorsSelect;
import org.genericsystem.reactor.gs.GSSelect.InstanceCompositeSelect;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlCheckBox;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

public class GSLinks {
	static final Logger log = LoggerFactory.getLogger(Tag.class);

	public static class LabelDisplayer extends GSSection {

		private final ObservableListExtractor observableListExtractor;
		private final boolean reverse;

		public LabelDisplayer(GSTag parent, ObservableListExtractor observableListExtractor, FlexDirection flexDirection, boolean reverse) {
			super(parent, flexDirection);
			this.observableListExtractor = observableListExtractor;
			this.reverse = reverse;
			content();
		}

		private void content() {
			new GSSection(this, reverse ? this.getReverseDirection() : this.getDirection()) {
				{
					style(this);
					select(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
					new GSSection(this, this.getDirection()) {
						{
							addStyle("justify-content", "center");
							addStyle("align-items", "center");
							addStyle("width", "100%");
							addStyle("height", "100%");
							new HtmlLabel<GenericModel>(this) {
								{
									select(gs -> !Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null);
									bindText(GenericModel::getString);
								}
							};
							new HtmlCheckBox<GenericModel>(this) {
								{
									addAttribute("disabled", "disabled");
									initProperty(ReactorStatics.CHECKED, model -> {
										assert !model.destroyed;
										return (Boolean) model.getGeneric().getValue();
									});
									bindOptionalBiDirectionalAttribute(ReactorStatics.CHECKED, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
									select(gs -> Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null);
								}
							};
						}
					};
				}
			};
			new GSSection(this, reverse ? this.getReverseDirection() : this.getDirection()) {
				{
					style(this);
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, observableListExtractor);
					new HtmlLabel<GenericModel>(this) {
						{
							bindText(GenericModel::getString);
						}
					};
				}
			};
		}

		public void style(Tag<?> tag) {
		}
	}

	public static class LinkDisplayer extends LabelDisplayer {

		public LinkDisplayer(GSTag parent, FlexDirection direction) {
			super(parent, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])), direction, true);
		}

		@Override
		public void style(Tag<?> tag) {
			tag.addStyle("overflow", "hidden");
			tag.addStyle("flex", "1");
			tag.addStyle("margin-right", "1px");
			tag.addStyle("margin-bottom", "1px");
			tag.addPrefixBinding(modelContext -> ((Model) modelContext).getObservableStyles(tag).put("background-color",
					"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(((GenericModel) modelContext).getGeneric().getMeta())) ? ((GenericModel) modelContext).getString().getValue() : "#dda5e2"));
		}
	}

	public static class LinkTitleDisplayer extends LabelDisplayer {

		public LinkTitleDisplayer(GSTag parent, ObservableListExtractor observableListExtractor, FlexDirection flexDirection) {
			super(parent, observableListExtractor, flexDirection, false);
		}

		@Override
		public void style(Tag<?> tag) {
			tag.addStyle("flex", "1");
			tag.addStyle("color", "#ffffff");
			tag.addStyle("background-color", "#ffa5a5");
			tag.addStyle("margin-right", "1px");
			tag.addStyle("margin-bottom", "1px");
		}
	}

	public static class LinkEditor extends GSSection {

		private final boolean reverse;

		public LinkEditor(GSTag parent, FlexDirection direction) {
			// TODO: filter only once.
			this(parent, direction, true);
		}

		private LinkEditor(GSTag parent, FlexDirection flexDirection, boolean reverse) {
			super(parent, flexDirection);
			this.reverse = reverse;
			content();
		}

		@Deprecated
		public void initInputSection(GSTag tag) {
			tag.select(gs -> !Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null);
		}

		@Deprecated
		public void initInputText(GSTag tag) {
			tag.initProperty(ReactorStatics.VALUE, model -> model.getGeneric().getValue());
			tag.bindActionToValueChangeListener(ReactorStatics.VALUE, (model, nva) -> {
				model.getGeneric().updateValue(nva);
			});
		}

		@Deprecated
		public void initCheckBox(GSTag tag) {
			tag.select(gs -> Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null);
			tag.initProperty(ReactorStatics.CHECKED, model -> (Boolean) model.getGeneric().getValue());
			tag.bindActionToValueChangeListener(ReactorStatics.CHECKED, (model, nva) -> model.getGeneric().updateValue(nva));
		}

		@Deprecated
		public void initRelations(GSTag tag) {
			tag.forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[3])));
			new InstanceCompositeSelect(tag) {
				{
					initComboBoxCommon(this);
					initComboBox(this);
				}
			};
			new HtmlLabel<GenericModel>(this) {
				{
					select(gs -> !gs[1].isReferentialIntegrityEnabled(pos(gs[1], gs[0])) ? gs[0] : null);
					bindText(GenericModel::getString);
				}
			};
		}

		@Deprecated
		public void initComboBoxCommon(GSSelect tag) {
			tag.select(gs -> gs[1].isReferentialIntegrityEnabled(pos(gs[1], gs[0])) ? gs[0] : null);
			tag.addPostfixBinding(model -> {
				if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
					model.getObservableStyles(tag).put("background-color", (String) model.getObservableValue(tag, ReactorStatics.SELECTION_STRING).getValue());
			});
			tag.optionElement.addPrefixBinding(model -> {
				if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
					model.getObservableStyles(tag.optionElement).put("background-color", model.getString().getValue());
			});
			tag.addStyle("width", "100%");
			tag.addStyle("height", "100%");
		}

		public void initComboBox(Tag<GenericModel> tag) {
			tag.addPostfixBinding(modelContext -> {
				int axe = pos(modelContext.getGenerics()[2], modelContext.getGenerics()[1]);
				tag.getProperty(ReactorStatics.SELECTION, modelContext).addListener((ov, ova, nva) -> modelContext.getGenerics()[2].updateComponent(((GenericModel) nva).getGeneric(), axe));
			});
		}

		public void inputActionButton(Tag<GenericModel> tag, GSInputText input) {

		}

		public void checkBoxActionButton(Tag<GenericModel> tag, HtmlCheckBox<GenericModel> checkbox) {

		}

		public void linkActionButton(Tag<GenericModel> tag) {

		}

		private void content() {
			new GSSection(this, reverse ? this.getReverseDirection() : this.getDirection()) {
				{
					style(this);
					select(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
					new GSSection(this, this.getDirection()) {
						{
							addStyle("flex", "1");
							addStyle("width", "100%");
							addStyle("height", "100%");
							addStyle("justify-content", "center");
							addStyle("align-items", "center");
							new GSSection(this, this.getDirection()) {
								{
									addStyle("flex", "1");
									addStyle("width", "100%");
									addStyle("height", "100%");
									GSInputText input = new GSInputText(this) {
										{
											initProperty(ReactorStatics.VALUE, model -> model.getGeneric().getValue());
											initInputText(this);
										}
									};
									initInputSection(this);
									inputActionButton(this, input);
								}
							};
							new GSSection(this, this.getDirection()) {
								{
									initCheckBox(this);
									addStyle("width", "100%");
									addStyle("height", "100%");
									new GSSection(this, this.getDirection()) {
										{
											addStyle("width", "100%");
											addStyle("height", "100%");
											addStyle("justify-content", "center");
											addStyle("align-items", "center");
											checkbox = new HtmlCheckBox<GenericModel>(this) {
												{
													bindOptionalBiDirectionalAttribute(ReactorStatics.CHECKED, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
												}
											};
										}
									};
									insertButton(this);
								}

								private HtmlCheckBox<GenericModel> checkbox;

								public void insertButton(Tag<GenericModel> tag) {
									checkBoxActionButton(tag, checkbox);
								}
							};
						}
					};
				}
			};
			new GSSection(this, this.getDirection()) {

				{
					select(gs -> gs[0].getComponents().size() >= 2 ? gs[0] : null);
					style(this);
					new GSSection(this, reverse ? this.getReverseDirection() : this.getDirection()) {
						{
							addStyle("flex", "1");
							addStyle("width", "100%");
							addStyle("height", "100%");
							addStyle("justify-content", "center");
							addStyle("align-items", "center");
							initRelations(this);
						}
					};
					linkActionButton(this);
				}
			};
		}

		public void style(Tag<?> tag) {
			tag.addStyle("flex", "1");
			tag.addStyle("color", "#ffffff");
			tag.addStyle("background-color", "#dda5e2");
			tag.addStyle("margin-right", "1px");
			tag.addStyle("margin-bottom", "1px");
		}

		protected int pos(Generic genericToUpdate, Generic oldComponent) {
			List<Generic> components = genericToUpdate.getComponents();
			int pos = 0;
			for (Generic component : components) {
				if (component.equals(oldComponent))
					break;
				pos++;
			}
			return pos;
		}
	}

	public static class LinkEditorWithRemoval extends LinkEditor {
		public LinkEditorWithRemoval(GSTag parent, FlexDirection direction) {
			super(parent, direction);
		}

		@Override
		public void inputActionButton(Tag<GenericModel> tag, GSInputText input) {
			new HtmlButton<GenericModel>(tag) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					setText("×");
					bindAction(GenericModel::remove);
				}
			};
		}

		@Override
		public void checkBoxActionButton(Tag<GenericModel> tag, HtmlCheckBox<GenericModel> checkbox) {
			inputActionButton(tag, null);
		}

		@Override
		public void linkActionButton(Tag<GenericModel> tag) {
			inputActionButton(tag, null);
		}
	}

	public static class LinkAdder extends LinkEditor {

		public LinkAdder(GSTag parent, FlexDirection direction) {
			super(parent, direction);
		}

		@Override
		public void initInputSection(GSTag tag) {
			tag.select(gs -> !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
		}

		@Override
		public void initCheckBox(GSTag tag) {
			tag.select(gs -> Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
			tag.bindOptionalBiDirectionalAttribute(ReactorStatics.VALUE, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
		}

		@Override
		public void initRelations(GSTag tag) {
			tag.forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[4])));
			new ColorsSelect(tag) {
				{
					initComboBoxCommon(this);
				}
			};
		}

		@Override
		public void inputActionButton(Tag<GenericModel> tag, GSInputText input) {
			new HtmlButton<GenericModel>(tag) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					setText("+");
					bindPostfixAction(model -> {
						// TODO: Add a binding somewhere so the value is correct (untested but probably same problem as for checkboxes).
						Property<Serializable> observable = input.getProperty(ReactorStatics.VALUE, model);
						model.getGenerics()[4].addHolder(model.getGenerics()[3], observable.getValue());
					});
				}
			};
		}

		@Override
		public void checkBoxActionButton(Tag<GenericModel> tag, HtmlCheckBox<GenericModel> checkbox) {
			new HtmlButton<GenericModel>(tag) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					setText("+");
					bindPostfixAction(model -> {
						// TODO: Add a binding somewhere so the value is correct (it’s always false currently).
						Property<Serializable> observable = checkbox.getProperty(ReactorStatics.VALUE, model);
						model.getGenerics()[4].addHolder(model.getGenerics()[3], observable.getValue());
					});
				}
			};
		}

		@Override
		public void linkActionButton(Tag<GenericModel> tag) {
			new HtmlButton<GenericModel>(tag) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					setText("+");
					bindAction(model -> {
						try {
							// TODO: Does not work currently.
							new HolderVisitor(model.getGenerics()[3]).visit(model);
							new ClearVisitor().visit(model);
						} catch (RollbackException e) {
							e.printStackTrace();
						}
					});
				}
			};
		}
	}
}
