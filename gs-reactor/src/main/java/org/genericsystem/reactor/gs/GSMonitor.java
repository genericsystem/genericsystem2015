package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.gstag.GSButton;
import org.genericsystem.reactor.model.GenericModel;

public class GSMonitor extends GSSection {

	public GSMonitor(GSTag parent) {
		this(parent, FlexDirection.ROW);
	}

	public GSMonitor(GSTag parent, FlexDirection direction) {
		super(parent, direction);
		addStyle("justify-content", "space-around");
		addStyle("padding", "10px");
		new GSButton(this) {
			{
				setText("Save");
				bindAction(GenericModel::flush);
			}
		};
		new GSButton(this) {
			{
				setText("Cancel");
				bindAction(GenericModel::cancel);
			}
		};
		// new GSButton(this) {
		// {
		// setText("Collect");
		// bindAction(model -> System.gc());
		// }
		// };
	}

}