package org.genericsystem.carcolor;

import org.genericsystem.reactor.gs.FlexDirection;
import org.genericsystem.reactor.gs.GSCellDisplayer.GSTagConstructor;
import org.genericsystem.reactor.gs.GSSection;
import org.genericsystem.reactor.gs.GSTag;

public class GSHeader extends GSSection {

	public GSHeader(GSTag parent, String string, GSTagConstructor tag1, String string1, GSTagConstructor tag2, String string2) {
		super(parent, FlexDirection.ROW);
		addStyle("justify-content", "space-around");
		addStyle("padding", "10px");

		if (tag1 != null) {
			GSTag leftTag = tag1.build(this);
			leftTag.addStyle("flex", "1");
			leftTag.setText(string1);
		} else {
			GSTag leftTag = new GSSection(this, FlexDirection.COLUMN);
			leftTag.addStyle("flex", "1");
		}
		new GenericH1Section(this, string) {
			{
				addStyle("flex", "3");
			}
		};
		if (tag2 != null) {
			GSTag rightTag = tag2.build(this);
			rightTag.addStyle("flex", "1");
			rightTag.setText(string2);
		} else {
			GSTag rightTag = new GSSection(this, FlexDirection.COLUMN);
			rightTag.addStyle("flex", "1");
		}
	};

}
