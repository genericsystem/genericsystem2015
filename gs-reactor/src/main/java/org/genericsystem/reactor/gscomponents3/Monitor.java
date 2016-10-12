package org.genericsystem.reactor.gscomponents3;

import org.genericsystem.reactor.htmltag.HtmlButton;
import org.genericsystem.reactor.htmltag.HtmlLabel;

import org.genericsystem.reactor.gscomponents3.Monitor.CancelButton;
import org.genericsystem.reactor.gscomponents3.Monitor.FlushButton;
import org.genericsystem.reactor.gscomponents3.Monitor.LastUpdateLabel;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Date;

import org.genericsystem.common.Statics;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.ReactorDependencies;
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

import javafx.beans.binding.Bindings;

@ReactorDependencies({ FlushButton.class, CancelButton.class, LastUpdateLabel.class })
@FlexDirectionStyle(FlexDirection.ROW)
@Style(name = "justify-content", value = "space-around")
@Style(name = "padding", value = "10px")
public class Monitor extends GSDiv {

	@ReactorDependencies({ FlushButton.class, CancelButton.class, MountButton.class, CacheLevel.class, UnmountButton.class, ShiftTSButton.class, LastUpdateLabel.class })
	public static class MonitorExtended extends Monitor {
	}

	@SetText("Save")
	@BindAction(FLUSH.class)
	public static class FlushButton extends HtmlButton {
	}

	@SetText("Cancel")
	@BindAction(CANCEL.class)
	public static class CancelButton extends HtmlButton {
	}

	public static class LastUpdateLabel extends HtmlLabel {

		@Override
		public void init() {
			bindText(context -> Bindings.createStringBinding(() -> {
				Long tsMs = (Long) context.getTsObservableValue().getValue() / Statics.MILLI_TO_NANOSECONDS;
				Date dateMs = new Date(tsMs);
				Instant instant = Instant.ofEpochMilli(dateMs.getTime());
				LocalDateTime ldt = LocalDateTime.ofInstant(instant, ZoneOffset.systemDefault());
				return "Last update : " + ldt.format(DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss"));
			}, context.getTsObservableValue()));
		}
	}

	@SetText("Collect")
	@BindAction(GC.class)
	public static class CollectButton extends HtmlButton {
	}

	@SetText("Mount")
	@BindAction(MOUNT.class)
	public static class MountButton extends HtmlButton {
	}

	public static class CacheLevel extends HtmlLabel {

		@Override
		public void init() {
			bindText(context -> Bindings.convert(context.getCacheLevelObservableValue()));
		}
	}

	@SetText("Unmount")
	@BindAction(UNMOUNT.class)
	public static class UnmountButton extends HtmlButton {
	}

	@SetText("ShiftTS")
	@BindAction(SHIFTTS.class)
	public static class ShiftTSButton extends HtmlButton {
	}
}
