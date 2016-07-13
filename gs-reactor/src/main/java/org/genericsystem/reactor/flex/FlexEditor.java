package org.genericsystem.reactor.flex;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.flex.FlexLinks.FlexLinkEditor;
import org.genericsystem.reactor.flex.FlexLinks.FlexLinkTitleDisplayer;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.GenericModel.StringExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class FlexEditor extends CompositeFlexSection {

	public FlexEditor(Tag<?> parent) {
		this(parent, FlexDirection.COLUMN);
	}

	public FlexEditor(Tag<?> parent, FlexDirection flexDirection) {
		super(parent, flexDirection);
		addStyle("flex", "1");
	}

	@Override
	protected void header() {
		new FlexSection(this, FlexEditor.this.getReverseDirection()) {
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

		new CompositeFlexSection(this, FlexEditor.this.getReverseDirection()) {
			{
				addStyle("flex", "1");
			}

			@Override
			protected void header() {
				new FlexSection(this, FlexEditor.this.getDirection()) {
					{
						addStyle("flex", "0.3");
						new FlexLinkTitleDisplayer(this, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1].getMeta())), FlexEditor.this.getDirection()) {
							{
								addStyle("flex", "1");
								addStyle("overflow", "hidden");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
							}
						};
					}
				};
			}

			@Override
			protected void sections() {
				new FlexSection(this, FlexEditor.this.getDirection()) {
					{
						addStyle("flex", "1");
						new FlexSection(this, FlexEditor.this.getReverseDirection()) {
							{
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
								addStyle("flex", "1");
								addStyle("overflow", "hidden");
								new FlexLinkEditor(this, FlexEditor.this.getDirection()) {
									{
										addStyle("flex", "1");
										forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.HOLDERS);
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
