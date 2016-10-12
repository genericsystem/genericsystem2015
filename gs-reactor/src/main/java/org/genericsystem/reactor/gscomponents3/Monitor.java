package org.genericsystem.reactor.gscomponents3;

import org.genericsystem.reactor.htmltag.HtmlButton;
import org.genericsystem.reactor.htmltag.HtmlLabel;

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
import org.genericsystem.reactor.gscomponents3.Monitor.LastUpdateLabel;
import org.genericsystem.reactor.model.ContextAction.CANCEL;
import org.genericsystem.reactor.model.ContextAction.FLUSH;
import org.genericsystem.reactor.model.ContextAction.GC;
import org.genericsystem.reactor.model.ContextAction.MOUNT;
import org.genericsystem.reactor.model.ContextAction.SHIFTTS;
import org.genericsystem.reactor.model.ContextAction.UNMOUNT;

import javafx.beans.binding.Bindings;

@ReactorDependencies({ HtmlButton.class, HtmlButton.class, LastUpdateLabel.class })
@SetText(path = HtmlButton.class, pos = 0, value = "Save")
@BindAction(path = HtmlButton.class, pos = 0, value = FLUSH.class)
@SetText(path = HtmlButton.class, pos = 1, value = "Cancel")
@BindAction(path = HtmlButton.class, pos = 1, value = CANCEL.class)
@FlexDirectionStyle(FlexDirection.ROW)
@Style(name = "justify-content", value = "space-around")
@Style(name = "padding", value = "10px")
public class Monitor extends GSDiv {

	@ReactorDependencies({ HtmlButton.class, HtmlButton.class, HtmlButton.class, CacheLevel.class, HtmlButton.class, HtmlButton.class, LastUpdateLabel.class/*, HtmlButton.class */ })
	@SetText(path = HtmlButton.class, pos = 2, value = "Mount")
	@BindAction(path = HtmlButton.class, pos = 2, value = MOUNT.class)
	@SetText(path = HtmlButton.class, pos = 3, value = "Unmount")
	@BindAction(path = HtmlButton.class, pos = 3, value = UNMOUNT.class)
	@SetText(path = HtmlButton.class, pos = 4, value = "ShiftTs")
	@BindAction(path = HtmlButton.class, pos = 4, value = SHIFTTS.class)
	@SetText(path = HtmlButton.class, pos = 5, value = "Collect")
	@BindAction(path = HtmlButton.class, pos = 5, value = GC.class)
	public static class MonitorExtended extends Monitor {
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

	public static class CacheLevel extends HtmlLabel {
		@Override
		public void init() {
			bindText(context -> Bindings.convert(context.getCacheLevelObservableValue()));
		}
	}
}
