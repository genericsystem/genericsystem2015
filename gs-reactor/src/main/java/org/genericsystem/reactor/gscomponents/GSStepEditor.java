package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.modelproperties.StepperDefaults;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gscomponents.GSSubcellDisplayer.GSSubcellEditor;
import org.genericsystem.reactor.gscomponents.GSSubcellDisplayer.InstanceLinkTitleDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor;

public class GSStepEditor extends GSEditor implements StepperDefaults {

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
				forEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
				addStyle("flex", "1");
				new InstanceLinkTitleDisplayer(this).addStyle("flex", "0.3");
				new GSAttributeOfInstanceEditor(this);
				new StepNavigator(this, flexDirection.reverse());
			}
		};
	}
}
