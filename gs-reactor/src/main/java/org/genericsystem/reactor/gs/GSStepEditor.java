package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.gs.GSSubcellDisplayer.GSSubcellEditor;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.InstanceLinkTitleDisplayer;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.InstanceTitleDisplayer;
import org.genericsystem.reactor.gstag.HtmlHyperLink;

public class GSStepEditor extends GSEditor implements SwitchDefaults {

	protected GSTag switchedTag;
	protected GSTag instanceNameTag;

	public GSStepEditor(GSTag parent) {
		this(parent, FlexDirection.COLUMN);
	}

	public GSStepEditor(GSTag parent, FlexDirection direction) {
		super(parent, direction);
	}

	@Override
	protected void sections() {
		instanceNameTag = new GSSection(this, getDirection()) {
			{
				addStyle("flex", "1");
				new InstanceTitleDisplayer(this).addStyle("flex", "0.3");
				new GSSubcellEditor(this);
				new StepNavigator(this, getReverseDirection());
			}
		};
		switchedTag = new GSSection(this, getDirection()) {
			{
				addStyle("flex", "1");
				new InstanceLinkTitleDisplayer(this).addStyle("flex", "0.3");
				new GSAttributeOfInstanceEditor(this);
				new StepNavigator(this, getReverseDirection());
			}
		};
	}

	public static class StepNavigator extends GSSection {

		public StepNavigator(GSTag parent, FlexDirection direction) {
			super(parent, direction);
			addStyle("justify-content", "space-between");
			new HtmlHyperLink(this) {
				{
					setText("<");
					bindAction(model -> prev(model));
				}
			};
			new HtmlHyperLink(this) {
				{
					setText(">");
					bindAction(model -> next(model));
				}
			};
		}
	}
}
