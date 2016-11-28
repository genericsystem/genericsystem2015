package org.genericsystem.reactor.gscomponents2;

import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.context.ContextAction.CANCEL;
import org.genericsystem.reactor.context.ContextAction.FLUSH;
import org.genericsystem.reactor.context.ContextAction.GC;
import org.genericsystem.reactor.context.ContextAction.MOUNT;
import org.genericsystem.reactor.context.ContextAction.SHIFTTS;
import org.genericsystem.reactor.context.ContextAction.UNMOUNT;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents2.Monitor2.CancelButton;
import org.genericsystem.reactor.gscomponents2.Monitor2.LastUpdateLabel;
import org.genericsystem.reactor.gscomponents2.Monitor2.SaveButton;

@Children({ SaveButton.class, CancelButton.class, LastUpdateLabel.class })
@FlexDirectionStyle(FlexDirection.ROW)
@Style(name = "justify-content", value = "space-around")
@Style(name = "padding", value = "10px")
public class Monitor2 extends FlexDiv {
	@Children({ SaveButton.class, CancelButton.class, MountButton.class, CacheLevelLabel.class, UnmountButton.class, ShiftTsButton.class, LastUpdateLabel.class/* , CollectButton.class */ })
	public static class MonitorExtended2 extends Monitor2 {

	}

	@SetText("Save")
	@BindAction(FLUSH.class)
	public static class SaveButton extends HtmlButton {
	}

	@SetText("Cancel")
	@BindAction(CANCEL.class)
	public static class CancelButton extends HtmlButton {
	}

	@BindText(TextBinding.LAST_UPDATE.class)
	public static class LastUpdateLabel extends HtmlLabel {
	}

	@SetText("Mount")
	@BindAction(MOUNT.class)
	public static class MountButton extends HtmlButton {
	}

	@SetText("Unmount")
	@BindAction(UNMOUNT.class)
	public static class UnmountButton extends HtmlButton {
	}

	@SetText("ShiftTs")
	@BindAction(SHIFTTS.class)
	public static class ShiftTsButton extends HtmlButton {
	}

	@BindText(TextBinding.CACHE_LEVEL.class)
	public static class CacheLevelLabel extends HtmlLabel {
	}

	@SetText("Collect")
	@BindAction(GC.class)
	public static class CollectButton extends HtmlButton {
	}
}
