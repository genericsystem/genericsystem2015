package org.genericsystem.reactor.flex;

import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotation.InstanceColorize;
import org.genericsystem.reactor.composite.CompositeSelect.InstanceCompositeSelect;
import org.genericsystem.reactor.html.HtmlCheckBox;
import org.genericsystem.reactor.html.HtmlInputText;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.GenericModel.StringExtractor;
import org.genericsystem.reactor.model.InputGenericModel;
import org.genericsystem.reactor.model.InputGenericModel.EditInputGenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.SelectorModel;

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
			new FlexSection<InputGenericModel>(this, reverse ? this.getReverseDirection() : this.getDirection()) {
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
							new HtmlCheckBox<InputGenericModel>(this) {
								{
									addAttribute("disabled", "disabled");
									initProperty(model -> model.getProperty(this, "checked"), model -> (Boolean) model.getGeneric().getValue());
									bindOptionalBiDirectionalAttribute(model -> model.getProperty(this, "checked"), "checked", "checked");
									select(gs -> Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null, InputGenericModel::new);
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
			new FlexSection<InputGenericModel>(this, reverse ? this.getReverseDirection() : this.getDirection()) {
				{
					style(this);
					select(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
					new FlexSection<GenericModel>(this, this.getDirection()) {
						{
							addStyle("justify-content", "center");
							addStyle("align-items", "center");
							addStyle("width", "100%");
							addStyle("height", "100%");
							new HtmlInputText<InputGenericModel>(this) {
								{
									select(gs -> !Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null, EditInputGenericModel::new);
									addStyle("width", "100%");
									addStyle("height", "100%");
									bindOptionalStyle("border-color", InputGenericModel::getInvalid, "red");
									bindTextBidirectional(InputGenericModel::getInputString);
									addPostfixBinding(model -> {
										model.getInputString().setValue(model.getStringConverter().toString(model.getGenerics()[0].getValue()));
										model.getInputString().addListener((ov, ova, nva) -> {
											model.getGenerics()[0].updateValue(model.getStringConverter().fromString(nva));
										});
									});
								}
							};
							new HtmlCheckBox<InputGenericModel>(this) {
								{
									initProperty(model -> model.getProperty(this, "checked"), model -> (Boolean) model.getGeneric().getValue());
									bindOptionalBiDirectionalAttribute(model -> model.getProperty(this, "checked"), "checked", "checked");
									bindOperation(model -> model.getProperty(this, "checked"), (model, nva) -> model.getGeneric().updateValue(nva));
									select(gs -> Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null, InputGenericModel::new);
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
					forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, observableListExtractor, SelectorModel::new);
					new InstanceCompositeSelect<SelectorModel>(this) {
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
}
