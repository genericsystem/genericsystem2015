package org.genericsystem.reactor.flex;

import java.util.Arrays;
import java.util.List;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.composite.CompositeSelect;
import org.genericsystem.reactor.composite.CompositeSelect.ColorsSelect;
import org.genericsystem.reactor.composite.CompositeSelect.InstanceCompositeSelect;
import org.genericsystem.reactor.html.HtmlCheckBox;
import org.genericsystem.reactor.html.HtmlInputText;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

import javafx.beans.binding.Bindings;
import javafx.util.StringConverter;

public class FlexLinks {

	public static class FlexLabelDisplayer extends GenericSection {

		private final ObservableListExtractor observableListExtractor;
		private final boolean reverse;

		public FlexLabelDisplayer(Tag<?> parent, ObservableListExtractor observableListExtractor, FlexDirection flexDirection, boolean reverse) {
			super(parent, flexDirection);
			this.observableListExtractor = observableListExtractor;
			this.reverse = reverse;
			content();
		}

		private void content() {
			new GenericSection(this, reverse ? this.getReverseDirection() : this.getDirection()) {
				{
					style(this);
					select(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
					new GenericSection(this, this.getDirection()) {
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
			new GenericSection(this, reverse ? this.getReverseDirection() : this.getDirection()) {
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

	public static class FlexLinkDisplayer extends FlexLabelDisplayer {

		public FlexLinkDisplayer(Tag<?> parent, FlexDirection direction) {
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

	public static class FlexLinkTitleDisplayer extends FlexLabelDisplayer {

		public FlexLinkTitleDisplayer(Tag<?> parent, ObservableListExtractor observableListExtractor, FlexDirection flexDirection) {
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

	public static class FlexLinkEditor extends GenericSection {

		private final boolean reverse;

		public FlexLinkEditor(Tag<?> parent, FlexDirection direction) {
			// TODO: filter only once.
			this(parent, direction, true);
		}

		private FlexLinkEditor(Tag<?> parent, FlexDirection flexDirection, boolean reverse) {
			super(parent, flexDirection);
			this.reverse = reverse;
			content();
		}

		public void initInputText(Tag<GenericModel> tag) {
			tag.select(gs -> !Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null);
			tag.initProperty(ReactorStatics.VALUE, model -> model.getGeneric().getValue());
			tag.bindOperation(ReactorStatics.VALUE, (model, nva) -> {
				model.getGeneric().updateValue(nva);
			});
		}

		public void initCheckBox(Tag<GenericModel> tag) {
			tag.select(gs -> Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null);
			tag.initProperty(ReactorStatics.CHECKED, model -> (Boolean) model.getGeneric().getValue());
			tag.bindOperation(ReactorStatics.CHECKED, (model, nva) -> model.getGeneric().updateValue(nva));
		}

		public void initRelations(Tag<GenericModel> tag) {
			tag.forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
			new InstanceCompositeSelect(tag) {
				{
					initComboBoxCommon(this);
					initComboBox(this);
				}
			};
			new HtmlLabel<GenericModel>(this) {
				{
					// TODO: Allow removal, and addition.
					select(gs -> !gs[1].isReferentialIntegrityEnabled(pos(gs[1], gs[0])) ? gs[0] : null);
					bindText(GenericModel::getString);
				}
			};
		}

		public void initComboBoxCommon(CompositeSelect tag) {
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
			initComboBox(tag);
		}

		public void initComboBox(Tag<GenericModel> tag) {
			tag.addPostfixBinding(modelContext -> {
				int axe = pos(modelContext.getGenerics()[2], modelContext.getGenerics()[1]);
				tag.getProperty(ReactorStatics.SELECTION, modelContext).addListener((ov, ova, nva) -> modelContext.getGenerics()[2].updateComponent(((GenericModel) nva).getGeneric(), axe));
			});
		}

		private void content() {
			new GenericSection(this, reverse ? this.getReverseDirection() : this.getDirection()) {
				{
					style(this);
					select(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
					new GenericSection(this, this.getDirection()) {
						{
							addStyle("width", "100%");
							addStyle("height", "100%");
							addStyle("justify-content", "center");
							addStyle("align-items", "center");
							new HtmlGenericInputText(this) {
								{
									initInputText(this);
								}
							};
							new HtmlCheckBox<GenericModel>(this) {
								{
									bindOptionalBiDirectionalAttribute(ReactorStatics.CHECKED, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
									initCheckBox(this);
								}
							};
						}
					};
				}
			};
			new GenericSection(this, reverse ? this.getReverseDirection() : this.getDirection()) {
				{
					style(this);
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					initRelations(this);
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

	public static class FlexLinkCreator extends FlexLinkEditor {

		public FlexLinkCreator(Tag<?> parent, FlexDirection direction) {
			super(parent, direction);
		}

		@Override
		public void initInputText(Tag<GenericModel> tag) {
			tag.select(gs -> !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
			tag.bindOperation(ReactorStatics.VALUE, (model, nva) -> {
				System.out.println("Input value changed, " + Arrays.asList(model.getGenerics()));
			});
		}

		@Override
		public void initCheckBox(Tag<GenericModel> tag) {
			tag.select(gs -> Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
			tag.bindOperation(ReactorStatics.CHECKED, (model, nva) -> {
				System.out.println("Checkbox value changed, " + Arrays.asList(model.getGenerics()));
			});
		}

		@Override
		public void initRelations(Tag<GenericModel> tag) {
			tag.forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[3])));
			new ColorsSelect(tag) {
				{
					initComboBoxCommon(this);
					initComboBox(this);
				}
			};
		}

		@Override
		public void initComboBox(Tag<GenericModel> tag) {
			tag.addPostfixBinding(modelContext -> {
				int axe = pos(modelContext.getGenerics()[2], modelContext.getGenerics()[1]);
				tag.getProperty(ReactorStatics.SELECTION, modelContext).addListener((ov, ova, nva) -> {
					System.out.println("Selection changed, " + Arrays.asList(modelContext.getGenerics()));
				});
			});
		}
	}

	public static class HtmlGenericInputText extends HtmlInputText<GenericModel> {
		public HtmlGenericInputText(Tag<?> parent) {
			super(parent);

			addStyle("width", "100%");
			addStyle("height", "100%");

			initProperty(ReactorStatics.CONVERTER, model -> getConverter(model));
			createProperty(ReactorStatics.VALUE);

			storeProperty(ReactorStatics.INVALID, model -> Bindings.createBooleanBinding(() -> {
				boolean required = model.getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION);
				String value = model.getObservableAttributes(this).get(ReactorStatics.VALUE);
				if (required && (value == null || value.trim().isEmpty()))
					return true;
				try {
					((StringConverter) getProperty(ReactorStatics.CONVERTER, model).getValue()).fromString(value);
					return false;
				} catch (Exception e) {
					return true;
				}
			}, model.getObservableAttributes(this)));
			bindOptionalStyleClass(ReactorStatics.INVALID, ReactorStatics.INVALID);

			bindBiDirectionalAttributeOnEnter(ReactorStatics.VALUE, ReactorStatics.VALUE, model -> (StringConverter) getProperty(ReactorStatics.CONVERTER, model).getValue());
		}

		public StringConverter<?> getConverter(GenericModel model) {
			Class<?> clazz = model.getGeneric().getMeta().getInstanceValueClassConstraint();
			if (clazz == null) {
				if (model.getGeneric().getValue() != null)
					clazz = model.getGeneric().getValue().getClass();
				else
					clazz = String.class;
			}
			return ApiStatics.STRING_CONVERTERS.get(clazz);
		}
	}
}
