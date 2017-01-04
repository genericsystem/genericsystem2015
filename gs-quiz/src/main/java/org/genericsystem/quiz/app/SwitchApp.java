package org.genericsystem.quiz.app;

import org.genericsystem.quiz.app.SwitchApp.SwitchDiv1;
import org.genericsystem.quiz.app.SwitchApp.SwitchDiv2;
import org.genericsystem.quiz.app.SwitchApp.SwitchDiv3;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.RootTagImpl;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

@Children({ SwitchDiv1.class, SwitchDiv2.class, SwitchDiv3.class })
public class SwitchApp extends RootTagImpl {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, SwitchApp.class, "/switchapp");
	}

	@Override
	public void init() {
		createNewInitializedProperty("selectedClass", context -> SwitchDiv2.class);
	}

	public static class NextAction implements ContextAction {

		@Override
		public void accept(Context context, Tag tag) {
			Class<?> from = tag.getParent().getClass();
			Class<?> to = null;
			if (SwitchDiv1.class.equals(from))
				to = SwitchDiv2.class;
			if (SwitchDiv2.class.equals(from))
				to = SwitchDiv3.class;
			if (SwitchDiv3.class.equals(from))
				to = SwitchDiv1.class;
			tag.getProperty("selectedClass", context).setValue(to);
		}

	}

	@Switch(NextSwitcher.class)
	@SetText("SwitchDiv1")
	@Children(HtmlButton.class)
	@BindAction(path = HtmlButton.class, value = NextAction.class)
	@SetText(path = HtmlButton.class, value = "Next")
	public static class SwitchDiv1 extends HtmlDiv {

	}

	@Switch(NextSwitcher.class)
	@SetText("SwitchDiv2")
	@Children(HtmlButton.class)
	@BindAction(path = HtmlButton.class, value = NextAction.class)
	@SetText(path = HtmlButton.class, value = "Next")
	public static class SwitchDiv2 extends HtmlDiv {

	}

	@Switch(NextSwitcher.class)
	@SetText("SwitchDiv3")
	@Children(HtmlButton.class)
	@BindAction(path = HtmlButton.class, value = NextAction.class)
	@SetText(path = HtmlButton.class, value = "First")
	public static class SwitchDiv3 extends HtmlDiv {

	}

	public static class NextSwitcher implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Property<?> selectedClassProperty = tag.getProperty("selectedClass", context);
			return Bindings.createBooleanBinding(() -> tag.getClass().equals(selectedClassProperty.getValue()), selectedClassProperty);
		}

	}
}
