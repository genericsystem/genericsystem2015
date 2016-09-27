package org.genericsystem.reactor.az;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.az.GSSubcellDisplayer.GSSubcellEditor;
import org.genericsystem.reactor.az.GSSubcellDisplayer.InstanceLinkTitleDisplayer;
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
		instanceNameTag = new GSDiv(this, flexDirection) {
			{
				addStyle("flex", "1");
				new InstanceLinkTitleDisplayer(this) {
					{
						addStyle("flex", "0.3");
						select(gs -> gs[1]);
					}
				};
				new GSSubcellEditor(this);
				new StepNavigator(this, flexDirection.reverse());
			}
		};
		switchedTag = new GSDiv(this, flexDirection) {
			{
				addStyle("flex", "1");
				new InstanceLinkTitleDisplayer(this).addStyle("flex", "0.3");
				new GSAttributeOfInstanceEditor(this);
				new StepNavigator(this, flexDirection.reverse());
			}
		};
	}
}
