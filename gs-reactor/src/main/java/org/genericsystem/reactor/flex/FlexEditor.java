package org.genericsystem.reactor.flex;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotation.InstanceColorize;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.GenericModel.StringExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class FlexEditor extends CompositeFlexSection<GenericModel> {

	public FlexEditor(Tag<?> parent) {
		this(parent, FlexDirection.COLUMN);
	}

	public FlexEditor(Tag<?> parent, FlexDirection flexDirection) {
		super(parent, flexDirection);
		addStyle("flex", "1");
	}

	@Override
	protected void header() {
		new FlexSection<GenericModel>(this, FlexEditor.this.getReverseDirection()) {
			{
				addStyle("flex", "0.3");
				addStyle("background-color", "#ffa500");
				addStyle("margin-right", "1px");
				addStyle("margin-bottom", "1px");
				addStyle("color", "red");
				addStyle("justify-content", "center");
				addStyle("align-items", "center");
				new HtmlH1<GenericModel>(this) {
					{
						bindText(GenericModel::getString);
					}
				};
			}
		};
	}

	@Override
	protected void sections() {

		new CompositeFlexSection<GenericModel>(this, FlexEditor.this.getReverseDirection()) {
			{
				addStyle("flex", "1");
			}

			@Override
			protected void header() {
				new FlexSection<GenericModel>(this, FlexEditor.this.getDirection()) {
					{
						addStyle("flex", "0.3");
						new CompositeFlexSection<GenericModel>(this, FlexEditor.this.getDirection()) {
							{
								addStyle("flex", "1");
								addStyle("overflow", "hidden");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
							}

							@Override
							public void header() {
								new FlexSection<GenericModel>(this, FlexEditor.this.getReverseDirection()) {
									{
										addStyle("flex", "1");
										addStyle("background-color", "#dda5e2");
										addStyle("margin-right", "1px");
										addStyle("margin-bottom", "1px");
										select_(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
										new HtmlLabel<GenericModel>(this).bindText(GenericModel::getString);
									}
								};
							}

							@Override
							public void sections() {
								new FlexSection<GenericModel>(this, FlexEditor.this.getReverseDirection()) {
									{
										addStyle("flex", "1");
										addPrefixBinding(modelContext -> modelContext.getObservableStyles(this).put("background-color",
												((GenericModel) modelContext).getGeneric().getMeta().getAnnotation(InstanceColorize.class) != null ? ((GenericModel) modelContext).getString().getValue() : "#dda5e2"));
										addStyle("margin-right", "1px");
										addStyle("margin-bottom", "1px");
										forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1].getMeta())));
										new HtmlLabel<GenericModel>(this).bindText(GenericModel::getString);
									}
								};
							}
						};
					}
				};
			}

			@Override
			protected void sections() {
				new FlexSection<GenericModel>(this, FlexEditor.this.getDirection()) {
					{
						addStyle("flex", "1");
						new FlexSection<GenericModel>(this, FlexEditor.this.getReverseDirection()) {
							{
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
								addStyle("flex", "1");
								addStyle("overflow", "hidden");
								new CompositeFlexSection<GenericModel>(this, FlexEditor.this.getDirection()) {
									{
										addStyle("flex", "1");
										forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.HOLDERS);
									}

									@Override
									public void header() {
										new FlexSection<GenericModel>(this, FlexEditor.this.getReverseDirection()) {
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
									}

									@Override
									public void sections() {
										new FlexSection<GenericModel>(this, FlexEditor.this.getReverseDirection()) {
											{
												addStyle("flex", "1");
												addStyle("overflow", "hidden");
												addPrefixBinding(modelContext -> modelContext.getObservableStyles(this).put("background-color",
														((GenericModel) modelContext).getGeneric().getMeta().getAnnotation(InstanceColorize.class) != null ? ((GenericModel) modelContext).getString().getValue() : "#dda5e2"));
												addStyle("margin-right", "1px");
												addStyle("margin-bottom", "1px");
												forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
												new HtmlLabel<GenericModel>(this).bindText(GenericModel::getString);
											}
										};
									}
								};
							}
						};
					}
				};
			};
		};
	}
}
