package org.genericsystem.reactor.flex;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotation.InstanceColorize;
import org.genericsystem.reactor.composite.CompositeSelect.InstanceCompositeSelect;
import org.genericsystem.reactor.html.HtmlCheckBox;
import org.genericsystem.reactor.html.HtmlInputText;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.GenericModel.StringExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor;

import javafx.beans.binding.Bindings;
import javafx.util.StringConverter;

public class FlexLinks {

	public static class FlexLabelDisplayer<M extends GenericModel> extends FlexSection<M> {

		private final ObservableListExtractor observableListExtractor;
		private final boolean reverse;

		public FlexLabelDisplayer(Tag<?> parent, ObservableListExtractor observableListExtractor, FlexDirection flexDirection, boolean reverse) {
			super(parent, flexDirection);
			this.observableListExtractor = observableListExtractor;
			this.reverse = reverse;
			content();
		}

		private void content() {
			new FlexSection<GenericModel>(this, reverse ? this.getReverseDirection() : this.getDirection()) {
				{
					style(this);
					select(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
					new FlexSection<GenericModel>(this, this.getDirection()) {
						{
							addStyle("justify-content", "center");
							addStyle("align-items", "center");
							addStyle("width", "100%");
							addStyle("height", "100%");
							new HtmlLabel<GenericModel>(this) {
								{
									select(gs -> !Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null, GenericModel::new);
									bindText(GenericModel::getString);
								}
							};
							new HtmlCheckBox<GenericModel>(this) {
								{
									addAttribute("disabled", "disabled");
									initProperty(ReactorStatics.CHECKED, model -> (Boolean) model.getGeneric().getValue());
									bindOptionalBiDirectionalAttribute(ReactorStatics.CHECKED, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
									select(gs -> Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null, GenericModel::new);
								}
							};
						}
					};
				}
			};
			new FlexSection<GenericModel>(this, reverse ? this.getReverseDirection() : this.getDirection()) {
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

	public static class FlexLinkDisplayer<M extends GenericModel> extends FlexLabelDisplayer<M> {

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
					((GenericModel) modelContext).getGeneric().getMeta().getAnnotation(InstanceColorize.class) != null ? ((GenericModel) modelContext).getString().getValue() : "#dda5e2"));
		}
	}

	public static class FlexLinkTitleDisplayer<M extends GenericModel> extends FlexLabelDisplayer<M> {

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

	public static class FlexLinkEditor<M extends GenericModel> extends FlexSection<M> {

		private final ObservableListExtractor observableListExtractor;
		private final boolean reverse;

		public FlexLinkEditor(Tag<?> parent, FlexDirection direction) {
			this(parent, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])), direction, true);
		}

		public FlexLinkEditor(Tag<?> parent, ObservableListExtractor observableListExtractor, FlexDirection flexDirection, boolean reverse) {
			super(parent, flexDirection);
			this.observableListExtractor = observableListExtractor;
			this.reverse = reverse;
			content();
		}

		private void content() {
			new FlexSection<GenericModel>(this, reverse ? this.getReverseDirection() : this.getDirection()) {
				{
					style(this);
					select(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
					new FlexSection<GenericModel>(this, this.getDirection()) {
						{
							addStyle("justify-content", "center");
							addStyle("align-items", "center");
							addStyle("width", "100%");
							addStyle("height", "100%");
							new HtmlGenericInputText<GenericModel>(this) {
								{
									select(gs -> !Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null, GenericModel::new);
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
									select(gs -> Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null, GenericModel::new);
								}
							};
						}
					};
				}
			};
			new FlexSection<GenericModel>(this, reverse ? this.getReverseDirection() : this.getDirection()) {
				{
					style(this);
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, observableListExtractor, GenericModel::new);
					new InstanceCompositeSelect<GenericModel>(this) {
						{
							addPostfixBinding(modelContext -> modelContext.getSelection().addListener((ov, ova, nva) -> modelContext.getGenerics()[1].updateComponent(nva.getGeneric(), 1)));
							addStyle("width", "100%");
							addStyle("height", "100%");
						}
					};
				}
			};
		}

		public void style(Tag<?> tag) {
			tag.addStyle("flex", "1");
			tag.addStyle("color", "#ffffff");
			tag.addStyle("background-color", "#ffa5a5");
			tag.addStyle("margin-right", "1px");
			tag.addStyle("margin-bottom", "1px");
		}
	}

	public static class HtmlGenericInputText<M extends GenericModel> extends HtmlInputText<M> {
		public HtmlGenericInputText(Tag<?> parent) {
			super(parent);

			addStyle("width", "100%");
			addStyle("height", "100%");

			initProperty("converter", model -> getConverter(model));

			setProperty(ReactorStatics.INVALID, model -> Bindings.createBooleanBinding(() -> {
				boolean required = model.getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION);
				String value = model.getObservableAttributes(this).get(ReactorStatics.VALUE);
				if (required && (value == null || value.trim().isEmpty()))
					return true;
				try {
					((StringConverter) model.getProperty(this, "converter").getValue()).fromString(value);
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
