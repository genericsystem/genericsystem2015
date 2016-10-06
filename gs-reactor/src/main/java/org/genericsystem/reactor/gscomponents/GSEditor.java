package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gscomponents.GSSubcellDisplayer.GSSubcellEditor;
import org.genericsystem.reactor.gscomponents.GSSubcellDisplayer.InstanceLinkTitleDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

import org.genericsystem.reactor.htmltag.HtmlH2;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class GSEditor extends TitledSection {

	public GSEditor(Tag parent) {
		this(parent, FlexDirection.COLUMN);
	}

	public GSEditor(Tag parent, FlexDirection flexDirection) {
		super(parent, flexDirection);
		addStyle("flex", "1");
	}

	@Override
	protected void titleHeader() {
		new GSDiv(this, FlexDirection.ROW) {
			{
				addStyle("flex", "0.3");
				addStyle("background-color", "#EA4500");
				addStyle("margin-right", "1px");
				addStyle("margin-bottom", "1px");
				addStyle("color", "White");
				addStyle("justify-content", "center");
				addStyle("align-items", "center");
				new HtmlH2(this) {
					{
						setStringExtractor(StringExtractor.TYPE_INSTANCE_EXTRACTOR);
						bindText();
					}
				};
			}
		};
	}

	@Override
	protected void content() {
		new GSComposite(this, flexDirection.reverse()) {
			{
				addStyle("flex", "1");
				addStyle("height", "100%");
			}

			@Override
			protected void header() {
				new GSDiv(this, flexDirection) {
					{
						addStyle("flex", "0.3");
						new InstanceLinkTitleDisplayer(this).select(gs -> gs[1]);
						new InstanceLinkTitleDisplayer(this) {
							{
								forEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
							}
						};
					}
				};
			}

			@Override
			protected void sections() {
				new GSDiv(this, flexDirection) {
					{
						addStyle("flex", "1");
						new GSSubcellEditor(this);
						new GSAttributeOfInstanceEditor(this) {
							{
								forEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
							}
						};
					}
				};
			};
		};
	}
}
