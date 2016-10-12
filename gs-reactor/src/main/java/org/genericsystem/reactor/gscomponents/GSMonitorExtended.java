package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;

import org.genericsystem.reactor.htmltag.HtmlButton;
import org.genericsystem.reactor.htmltag.HtmlLabel;

import javafx.beans.binding.Bindings;

public class GSMonitorExtended extends GSMonitor {

	public GSMonitorExtended(Tag parent) {
		this(parent, FlexDirection.ROW);
	}

	public GSMonitorExtended(Tag parent, FlexDirection direction) {
		super(parent, direction);
	}

	@Override
	protected void middlePart() {
		new HtmlButton(this) {
			{
				setText("Mount");
				bindAction(Context::mount);
			}
		};
		new HtmlLabel(this) {
			{
				bindText(context -> Bindings.convert(context.getCacheLevelObservableValue()));
			}
		};
		new HtmlButton(this) {
			{
				setText("UnMount");
				bindAction(Context::unmount);
			}
		};
		new HtmlButton(this) {
			{
				setText("ShiftTS");
				bindAction(Context::shiftTs);
			}
		};
	}
}
