package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.gs.GSSubcellDisplayer.GSSubcellEditor;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.InstanceLinkTitleDisplayer;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.InstanceTitleDisplayer;
import org.genericsystem.reactor.gstag.HtmlH2;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class GSEditor extends GSComposite {

	public GSEditor(GSTag parent) {
		this(parent, FlexDirection.COLUMN);
	}

	public GSEditor(GSTag parent, FlexDirection flexDirection) {
		super(parent, flexDirection);
		addStyle("flex", "1");
	}

	@Override
	protected void header() {
		new GSSection(this, GSEditor.this.getReverseDirection()) {
			{
				addStyle("flex", "0.3");
				addStyle("background-color", "#ffa500");
				addStyle("margin-right", "1px");
				addStyle("margin-bottom", "1px");
				addStyle("color", "red");
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
	protected void sections() {

		new GSComposite(this, GSEditor.this.getReverseDirection()) {
			{
				addStyle("flex", "1");
			}

			@Override
			protected void header() {
				new GSSection(this, GSEditor.this.getDirection()) {
					{
						addStyle("flex", "0.3");
						new InstanceTitleDisplayer(this);
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
				new GSSection(this, GSEditor.this.getDirection()) {
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
