package org.genericsystem.reactor.flex;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.annotation.InstanceColorize;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.model.CompositeModel;
import org.genericsystem.reactor.model.CompositeModel.StringExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class FlexEditor extends CompositeFlexElement<CompositeModel> {

	public FlexEditor(Element<?> parent) {
		this(parent, FlexTag.SECTION);
	}

	public FlexEditor(Element<?> parent, FlexTag tag) {
		this(parent, tag, FlexDirection.COLUMN);
	}

	public FlexEditor(Element<?> parent, FlexTag tag, FlexDirection flexDirection) {
		super(parent, tag, flexDirection);
		addStyle("flex", "1");
	}

	@Override
	protected void header() {
		new FlexElement<CompositeModel>(this, FlexTag.HEADER, FlexEditor.this.getReverseDirection()) {
			{
				addStyle("flex", "1");
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

	@Override
	protected void sections() {

		new CompositeFlexElement<CompositeModel>(this, FlexTag.SECTION, FlexEditor.this.getReverseDirection()) {
			{
				addStyle("flex", "1");
				forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
			}

			@Override
			protected void header() {
				new CompositeFlexElement<CompositeModel>(this, FlexTag.SECTION, FlexEditor.this.getDirection()) {
					{
						addStyle("flex", "1");
					}

					@Override
					public void header() {
						new FlexElement<CompositeModel>(this, FlexTag.HEADER, FlexEditor.this.getReverseDirection()) {
							{
								addStyle("flex", "1");
								addStyle("background-color", "#dda5e2");
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								select_(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
								new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
							}
						};
					}

					@Override
					public void sections() {
						new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexEditor.this.getReverseDirection()) {
							{
								addStyle("flex", "1");
								addPrefixBinding(modelContext -> modelContext.getObservableStyles(this).put("background-color",
										modelContext.<CompositeModel> getModel().getGeneric().getMeta().getAnnotation(InstanceColorize.class) != null ? modelContext.<CompositeModel> getModel().getString().getValue() : "#dda5e2"));
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1].getMeta())));
								new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
							}
						};
					}
				};

			}

			@Override
			protected void sections() {
				new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexEditor.this.getReverseDirection()) {
					{
						addStyle("flex", "1");
						new CompositeFlexElement<CompositeModel>(this, FlexTag.SECTION, FlexEditor.this.getDirection()) {
							{
								addStyle("flex", "1");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.HOLDERS);
							}

							@Override
							public void header() {
								new FlexElement<CompositeModel>(this, FlexTag.HEADER, FlexEditor.this.getReverseDirection()) {
									{
										addStyle("flex", "1");
										addStyle("background-color", "#dda5e2");
										addStyle("margin-right", "1px");
										addStyle("margin-bottom", "1px");
										select_(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
										new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
									}
								};
							}

							@Override
							public void sections() {
								new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexEditor.this.getReverseDirection()) {
									{
										addStyle("flex", "1");
										addPrefixBinding(modelContext -> modelContext.getObservableStyles(this).put("background-color",
												modelContext.<CompositeModel> getModel().getGeneric().getMeta().getAnnotation(InstanceColorize.class) != null ? modelContext.<CompositeModel> getModel().getString().getValue() : "#dda5e2"));
										addStyle("margin-right", "1px");
										addStyle("margin-bottom", "1px");
										forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
										new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
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
