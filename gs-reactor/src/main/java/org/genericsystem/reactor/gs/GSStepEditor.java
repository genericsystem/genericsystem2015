package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.gs.GSSubcellDisplayer.InstanceLinkTitleDisplayer;
import org.genericsystem.reactor.gstag.GSHyperLink;

public class GSStepEditor extends GSEditor implements SwitchDefaults {

	private GSTag switchedTag;

	public GSStepEditor(GSTag parent) {
		this(parent, FlexDirection.COLUMN);
	}

	public GSStepEditor(GSTag parent, FlexDirection direction) {
		super(parent, direction);
	}

	@Override
	public GSTag getSwitchedTag() {
		return switchedTag;
	}

	@Override
	protected void sections() {
		switchedTag = new GSSection(this, getDirection()) {
			{
				addStyle("flex", "1");
				new InstanceLinkTitleDisplayer(this).addStyle("flex", "0.3");
				new GSAttributeOfInstanceEditor(this);
				new GSSection(this, getReverseDirection()) {
					{
						addStyle("justify-content", "space-between");
						new GSHyperLink(this) {
							{
								setText("<");
								bindAction(model -> prev(model));
							}
						};
						new GSHyperLink(this) {
							{
								setText(">");
								bindAction(model -> next(model));
							}
						};
					}
				};
			}
		};
	}
}
