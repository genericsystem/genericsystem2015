package org.genericsystem.reactor.flex;

import java.util.List;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
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

		private final ObservableListExtractor observableListExtractor;
		private final boolean reverse;

		public FlexLinkEditor(Tag<?> parent, FlexDirection direction) {
			// TODO: filter only once.
			this(parent, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])), direction, true);
		}

		public FlexLinkEditor(Tag<?> parent, ObservableListExtractor observableListExtractor, FlexDirection flexDirection, boolean reverse) {
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
							addStyle("width", "100%");
							addStyle("height", "100%");
							addStyle("justify-content", "center");
							addStyle("align-items", "center");
							new HtmlGenericInputText(this) {
								{
									select(gs -> !Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null);
									initProperty(ReactorStatics.VALUE, model -> model.getGeneric().getValue());
									bindOperation(ReactorStatics.VALUE, (model, nva) -> {
										model.getGeneric().updateValue(nva);
									});
								}
							};
							new HtmlCheckBox<GenericModel>(this) {
								{
									initProperty(ReactorStatics.CHECKED, model -> (Boolean) model.getGeneric().getValue());
									bindOptionalBiDirectionalAttribute(ReactorStatics.CHECKED, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
									bindOperation(ReactorStatics.CHECKED, (model, nva) -> model.getGeneric().updateValue(nva));
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
					new InstanceCompositeSelect(this) {
						{
							select(gs -> gs[1].isReferentialIntegrityEnabled(pos(gs[1], gs[0])) ? gs[0] : null);
							addPostfixBinding(modelContext -> {
								int axe = pos(modelContext.getGenerics()[2], modelContext.getGenerics()[1]);
								modelContext.getProperty(this, ReactorStatics.SELECTION).addListener((ov, ova, nva) -> modelContext.getGenerics()[2].updateComponent(((GenericModel) nva).getGeneric(), axe));
							});
							// addPrefixBinding(model -> {
							// if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta()))) {
							// Map<String, String> map = model.getObservableStyles(this);
							// ChangeListener<String> listener = (o, old, newValue) -> map.put("background-color", newValue);
							// ObservableValue<String> observable = model.getSelectionString();
							// observable.addListener(listener);
							// map.put("background-color", observable.getValue());
							// }
							// });
							addPostfixBinding(model -> {
								if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
									model.getObservableStyles(this).put("background-color", (String) model.getObservableValue(this, ReactorStatics.SELECTION_STRING).getValue());
							});
							optionElement.addPrefixBinding(model -> {
								if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
									model.getObservableStyles(optionElement).put("background-color", model.getString().getValue());
							});
							addStyle("width", "100%");
							addStyle("height", "100%");
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

				int pos(Generic genericToUpdate, Generic oldComponent) {
					List<Generic> components = genericToUpdate.getComponents();
					int pos = 0;
					for (Generic component : components) {
						if (component.equals(oldComponent))
							break;
						pos++;
					}
					return pos;
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
	}

	public static class HtmlGenericInputText extends HtmlInputText<GenericModel> {
		public HtmlGenericInputText(Tag<?> parent) {
			super(parent);

			addStyle("width", "100%");
			addStyle("height", "100%");

			initProperty(ReactorStatics.CONVERTER, model -> getConverter(model));

			storeProperty(ReactorStatics.INVALID, model -> Bindings.createBooleanBinding(() -> {
				boolean required = model.getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION);
				String value = model.getObservableAttributes(this).get(ReactorStatics.VALUE);
				if (required && (value == null || value.trim().isEmpty()))
					return true;
				try {
					((StringConverter) model.getProperty(this, ReactorStatics.CONVERTER).getValue()).fromString(value);
					return false;
				} catch (Exception e) {
					return true;
				}
			}, model.getObservableAttributes(this)));
			bindOptionalStyleClass(ReactorStatics.INVALID, ReactorStatics.INVALID);

			bindBiDirectionalAttribute(ReactorStatics.VALUE, ReactorStatics.VALUE, model -> (StringConverter) model.getProperty(this, "converter").getValue());
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
