package org.genericsystem.reactor.gs;

import java.io.Serializable;
import java.util.List;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.Visitor.ClearVisitor;
import org.genericsystem.reactor.Visitor.HolderVisitor;
import org.genericsystem.reactor.gs.GSSelect.ColorsSelect;
import org.genericsystem.reactor.gs.GSSelect.InstanceCompositeSelect;
import org.genericsystem.reactor.gstag.GSButton;
import org.genericsystem.reactor.gstag.GSCheckBox;
import org.genericsystem.reactor.gstag.GSLabel;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import javafx.beans.property.Property;

public class GSLinks {
	static final Logger log = LoggerFactory.getLogger(Tag.class);

	public static class LabelDisplayer extends GSSection {

		private final ObservableListExtractor observableListExtractor;

		public LabelDisplayer(GSTag parent, ObservableListExtractor observableListExtractor) {
			super(parent, FlexDirection.ROW);
			this.observableListExtractor = observableListExtractor;
			content();
		}

		private void content() {
			new GSSection(this, FlexDirection.ROW) {
				{
					style(this);
					select(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
					new GSLabel(this) {
						{
							select(gs -> !Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null);
							bindText(GenericModel::getString);
						}
					};
					new GSCheckBox(this) {
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
			new GSSection(this, FlexDirection.ROW) {
				{
					style(this);
					forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, observableListExtractor);
					new GSLabel(this) {
						{
							bindText(GenericModel::getString);
						}
					};
				}
			};
		}

		public void style(Tag<?> tag) {
			tag.addStyle("justify-content", "center");
			tag.addStyle("align-items", "center");
			tag.addStyle("flex", "1");
			tag.addStyle("margin-right", "1px");
			tag.addStyle("margin-bottom", "1px");
			tag.addStyle("overflow", "hidden");
		}
	}

	public static class LinkDisplayer extends LabelDisplayer {

		public LinkDisplayer(GSTag parent) {
			super(parent, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
		}

		@Override
		public void style(Tag<?> tag) {
			super.style(tag);
			tag.addPrefixBinding(modelContext -> ((Model) modelContext).getObservableStyles(tag).put("background-color",
					"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(((GenericModel) modelContext).getGeneric().getMeta())) ? ((GenericModel) modelContext).getString().getValue() : "#dda5e2"));
		}
	}

	public static class LinkTitleDisplayer extends LabelDisplayer {

		public LinkTitleDisplayer(GSTag parent, ObservableListExtractor observableListExtractor) {
			super(parent, observableListExtractor);
		}

		@Override
		public void style(Tag<?> tag) {
			super.style(tag);
			tag.addStyle("color", "#ffffff");
			tag.addStyle("background-color", "#ffa5a5");
		}
	}

	public static class LinkEditor extends GSSection {

		public LinkEditor(GSTag parent) {
			// TODO: filter only once.
			super(parent, FlexDirection.ROW);
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
			tag.createNewProperty(ReactorStatics.CHECKED);
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
			new GSLabel(this) {
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

		public void inputActionButton(GSTag tag, GSInputTextWithConversion input) {

		}

		public void checkBoxActionButton(GSTag tag, GSCheckBox checkbox) {

		}

		public void linkActionButton(GSTag tag) {

		}

		private void content() {
			new GSSection(this, FlexDirection.ROW) {
				{
					style(this);
					select(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
					new GSSection(this, FlexDirection.ROW) {
						{
							addStyle("flex", "1");
							addStyle("width", "100%");
							addStyle("height", "100%");
							GSInputTextWithConversion input = new GSInputTextWithConversion(this) {
								{
									initProperty(ReactorStatics.VALUE, model -> model.getGeneric().getValue());
									initInputText(this);
								}
							};
							initInputSection(this);
							inputActionButton(this, input);
						}
					};
					new GSSection(this, FlexDirection.ROW) {
						{
							initCheckBox(this);
							addStyle("width", "100%");
							addStyle("height", "100%");
							new GSSection(this, FlexDirection.ROW) {
								{
									addStyle("width", "100%");
									addStyle("height", "100%");
									addStyle("justify-content", "center");
									addStyle("align-items", "center");
									checkbox = new GSCheckBox(this) {
										{
											bindOptionalBiDirectionalAttribute(ReactorStatics.CHECKED, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
										}
									};
								}
							};
							insertButton(this);
						}

						private GSCheckBox checkbox;

						public void insertButton(GSTag tag) {
							checkBoxActionButton(tag, checkbox);
						}
					};
				}
			};
			new GSSection(this, FlexDirection.ROW) {

				{
					style(this);
					select(gs -> gs[0].getComponents().size() >= 2 ? gs[0] : null);
					new GSSection(this, FlexDirection.ROW) {
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
		public LinkEditorWithRemoval(GSTag parent) {
			super(parent);
		}

		@Override
		public void inputActionButton(GSTag tag, GSInputTextWithConversion input) {
			new GSButton(tag) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					setText("×");
					bindAction(GenericModel::remove);
				}
			};
		}

		@Override
		public void checkBoxActionButton(GSTag tag, GSCheckBox checkbox) {
			inputActionButton(tag, null);
		}

		@Override
		public void linkActionButton(GSTag tag) {
			inputActionButton(tag, null);
		}
	}

	public static class LinkAdder extends LinkEditor {

		public LinkAdder(GSTag parent) {
			super(parent);
		}

		@Override
		public void initInputSection(GSTag tag) {
			tag.select(gs -> !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
		}

		@Override
		public void initCheckBox(GSTag tag) {
			tag.createNewProperty(ReactorStatics.VALUE);
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
		public void inputActionButton(GSTag tag, GSInputTextWithConversion input) {
			new GSButton(tag) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					setText("+");
					bindAction(model -> {
						// TODO: Add a binding somewhere so the value is correct (untested but probably same problem as for checkboxes).
						Property<Serializable> observable = input.getProperty(ReactorStatics.VALUE, model);
						model.getGenerics()[4].addHolder(model.getGenerics()[3], observable.getValue());
					});
				}
			};
		}

		@Override
		public void checkBoxActionButton(GSTag tag, GSCheckBox checkbox) {
			new GSButton(tag) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					setText("+");
					bindAction(model -> {
						// TODO: Add a binding somewhere so the value is correct (it’s always false currently).
						Property<Serializable> observable = checkbox.getProperty(ReactorStatics.VALUE, model);
						model.getGenerics()[4].addHolder(model.getGenerics()[3], observable.getValue());
					});
				}
			};
		}

		@Override
		public void linkActionButton(GSTag tag) {
			new GSButton(tag) {
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
