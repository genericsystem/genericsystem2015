package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gstag.HtmlButton;

public class GSMonitor extends GSDiv {

	public GSMonitor(Tag parent) {
		this(parent, FlexDirection.ROW);
	}

	public GSMonitor(Tag parent, FlexDirection direction) {
		super(parent, direction);
		addStyle("justify-content", "space-around");
		addStyle("padding", "10px");
		new HtmlButton(this) {
			{
				setText("Save");
				bindAction(Context::flush);
			}
		};
		new HtmlButton(this) {
			{
				setText("Cancel");
				bindAction(Context::cancel);
			}
		};
		// new HtmlButton(this) {
		// {
		// setText("traverse");
		// bindAction(Context::traverse);
		// }
		// };
		// new GSButton(this) {
		// {
		// setText("Collect");
		// bindAction(model -> System.gc());
		// }
		// };
	}

}