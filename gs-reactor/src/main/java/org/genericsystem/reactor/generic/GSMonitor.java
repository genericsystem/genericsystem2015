package org.genericsystem.reactor.generic;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.model.GenericModel;

public class GSMonitor extends GSSection {

	public GSMonitor(Tag<?> parent) {
		this(parent, FlexDirection.ROW);
	}

	public GSMonitor(Tag<?> parent, FlexDirection direction) {
		super(parent, direction);
		addStyle("justify-content", "space-around");
		addStyle("padding", "10px");
		new HtmlButton<GenericModel>(this) {
			{
				setText("Save");
				bindAction(GenericModel::flush);
			}
		};
		new HtmlButton<GenericModel>(this) {
			{
				setText("Cancel");
				bindAction(GenericModel::cancel);
			}
		};
		// new HtmlButton<GenericModel>(this) {
		// {
		// setText("Collect");
		// bindAction(model -> System.gc());
		// }
		// };
	}

}