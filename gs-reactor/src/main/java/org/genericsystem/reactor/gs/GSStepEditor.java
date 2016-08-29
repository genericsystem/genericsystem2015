package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.GSSubcellEditor;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.InstanceLinkTitleDisplayer;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.InstanceTitleDisplayer;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.modelproperties.SwitchDefaults;

public class GSStepEditor extends GSEditor implements SwitchDefaults {

	protected Tag switchedTag;
	protected Tag instanceNameTag;

	public GSStepEditor(Tag parent) {
		this(parent, FlexDirection.COLUMN);
	}

	public GSStepEditor(Tag parent, FlexDirection direction) {
		super(parent, direction);
	}

	@Override
	protected void content() {
		instanceNameTag = new GSSection(this, flexDirection) {
			{
				addStyle("flex", "1");
				new InstanceTitleDisplayer(this).addStyle("flex", "0.3");
				new GSSubcellEditor(this);
				new StepNavigator(this, flexDirection.reverse());
			}
		};
		switchedTag = new GSSection(this, flexDirection) {
			{
				addStyle("flex", "1");
				new InstanceLinkTitleDisplayer(this).addStyle("flex", "0.3");
				new GSAttributeOfInstanceEditor(this);
				new StepNavigator(this, flexDirection.reverse());
			}
		};
	}

	public static class StepNavigator extends GSSection {

		public StepNavigator(Tag parent, FlexDirection direction) {
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
