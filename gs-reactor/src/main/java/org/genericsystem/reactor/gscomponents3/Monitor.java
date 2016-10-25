package org.genericsystem.reactor.gscomponents3;

import org.genericsystem.reactor.htmltag.HtmlButton;
import org.genericsystem.reactor.htmltag.HtmlLabel;

import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.model.ContextAction.CANCEL;
import org.genericsystem.reactor.model.ContextAction.FLUSH;
import org.genericsystem.reactor.model.ContextAction.GC;
import org.genericsystem.reactor.model.ContextAction.MOUNT;
import org.genericsystem.reactor.model.ContextAction.SHIFTTS;
import org.genericsystem.reactor.model.ContextAction.UNMOUNT;
import org.genericsystem.reactor.model.TextBinding;

@Children({ HtmlButton.class, HtmlLabel.class, HtmlButton.class })
@SetText(path = HtmlButton.class, pos = 0, value = "Save")
@BindAction(path = HtmlButton.class, pos = 0, value = FLUSH.class)
@SetText(path = HtmlButton.class, pos = 1, value = "Cancel")
@BindAction(path = HtmlButton.class, pos = 1, value = CANCEL.class)
@BindText(path = HtmlLabel.class, pos = 0, value = TextBinding.LAST_UPDATE.class)
@FlexDirectionStyle(FlexDirection.ROW)
@Style(name = "justify-content", value = "space-around")
@Style(name = "padding", value = "10px")
public class Monitor extends GSDiv {

	@Children({ HtmlButton.class, HtmlButton.class, HtmlButton.class, HtmlLabel.class, HtmlButton.class, HtmlButton.class, HtmlLabel.class/* , HtmlButton.class */ })
	@SetText(path = HtmlButton.class, pos = 2, value = "Mount")
	@BindAction(path = HtmlButton.class, pos = 2, value = MOUNT.class)
	@SetText(path = HtmlButton.class, pos = 3, value = "Unmount")
	@BindAction(path = HtmlButton.class, pos = 3, value = UNMOUNT.class)
	@SetText(path = HtmlButton.class, pos = 4, value = "ShiftTs")
	@BindAction(path = HtmlButton.class, pos = 4, value = SHIFTTS.class)
	@SetText(path = HtmlButton.class, pos = 5, value = "Collect")
	@BindAction(path = HtmlButton.class, pos = 5, value = GC.class)
	@BindText(path = HtmlLabel.class, pos = 1, value = TextBinding.CACHE_LEVEL.class)
	public static class MonitorExtended extends Monitor {
	}
}
