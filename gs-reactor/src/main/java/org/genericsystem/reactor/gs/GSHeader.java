package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.gs.GSSubcellDisplayer.GSTagConstructor;

public class GSHeader extends GSSection {

	public GSHeader(GSTag parent, String string, GSTagConstructor tag1, String string1, GSTagConstructor tag2, String string2) {
		super(parent, FlexDirection.ROW);
		addStyle("justify-content", "space-around");
		addStyle("padding", "10px");
		if (tag1 != null) {
			GSTag leftTag = tag1.build(this);
			leftTag.setText(string1);
			leftTag.addStyle("flex", "1");
		} else {
			GSTag leftTag = new GSSection(this, FlexDirection.COLUMN);
			leftTag.addStyle("flex", "1");
		}
		new GenericH2Section(this, string) {
			{
				addStyle("flex", "3");
				addStyle("align-items", "center");
			}
		};
		if (tag2 != null) {
			GSTag rightTag = tag2.build(this);
			rightTag.setText(string2);
			rightTag.addStyle("flex", "1");
		} else {
			GSTag rightTag = new GSSection(this, FlexDirection.COLUMN);
			rightTag.addStyle("flex", "1");
		}
	};

}