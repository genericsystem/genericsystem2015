package org.genericsystem.quiz.app;

import org.genericsystem.common.Root;
import org.genericsystem.quiz.app.SwitchApp.SwitchAppScript;
import org.genericsystem.quiz.app.SwitchApp.SwitchDiv1;
import org.genericsystem.quiz.app.SwitchApp.SwitchDiv2;
import org.genericsystem.quiz.app.SwitchApp.SwitchDiv3;
import org.genericsystem.quiz.app.SwitchApp.SwitchDiv4;
import org.genericsystem.quiz.app.SwitchApp.SwitchDiv5;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Switcher;
import org.genericsystem.reactor.annotations.SwitcherSubSteps;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.Script;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.gscomponents.RootTagImpl;
import org.genericsystem.reactor.gscomponents.SwitchChildDiv;
import org.genericsystem.reactor.gscomponents.SwitchChildDiv.Next;
import org.genericsystem.reactor.gscomponents.SwitchChildDiv.Prev;

@Switcher(SwitchDiv1.class)
@RunScript(SwitchAppScript.class)
@Children({ SwitchDiv1.class, SwitchDiv2.class, SwitchDiv3.class, SwitchDiv4.class, SwitchDiv5.class })
public class SwitchApp extends RootTagImpl {

	public static class SwitchAppScript implements Script {

		@Override
		public void run(Root engine) {
			engine.setInstance("Car");
			engine.setInstance("Color");
			engine.getCurrentCache().flush();
		}
	}

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, SwitchApp.class, "/switchapp");
	}

	@SetText("Step 1")
	@Next(SwitchDiv2.class)
	public static class SwitchDiv1 extends SwitchChildDiv {

	}

	@SetText("Step 2")
	@Prev(SwitchDiv1.class)
	@Next(SwitchDiv3.class)
	public static class SwitchDiv2 extends SwitchChildDiv {

	}

	@SetText("Step 3")
	@Prev(SwitchDiv2.class)
	@Next(SwitchDiv4.class)
	public static class SwitchDiv3 extends SwitchChildDiv {

	}

	@SetText("Step 4")
	@Prev(SwitchDiv3.class)
	@Next(SwitchDiv5.class)
	@SwitcherSubSteps(ObservableListExtractor.INSTANCES.class)
	public static class SwitchDiv4 extends SwitchChildDiv {

	}

	@SetText("Step 5")
	@Prev(SwitchDiv4.class)
	@SwitcherSubSteps(ObservableListExtractor.INSTANCES.class)
	public static class SwitchDiv5 extends SwitchChildDiv {

	}

//	@Children({ SwitchDiv1.class, SwitchDiv2.class, SwitchDiv3.class })
//	public class SwitchApp extends RootTagImpl {
//
//		public static void main(String[] mainArgs) {
//			ApplicationServer.startSimpleGenericApp(mainArgs, SwitchApp.class, "/switchapp");
//		}
//
//		@Override
//		public void init() {
//			createNewInitializedProperty("selectedClass", context -> SwitchDiv2.class);
//		}
//
//		public static class NextAction implements ContextAction {
//
//			@Override
//			public void accept(Context context, Tag tag) {
//				Class<?> from = tag.getParent().getClass();
//				Class<?> to = null;
//				if (SwitchDiv1.class.equals(from))
//					to = SwitchDiv2.class;
//				if (SwitchDiv2.class.equals(from))
//					to = SwitchDiv3.class;
//				if (SwitchDiv3.class.equals(from))
//					to = SwitchDiv1.class;
//				tag.getProperty("selectedClass", context).setValue(to);
//			}
//
//		}
//
//		@Switch(NextSwitcher.class)
//		@SetText("SwitchDiv1")
//		@Children(HtmlButton.class)
//		@BindAction(path = HtmlButton.class, value = NextAction.class)
//		@SetText(path = HtmlButton.class, value = "Next")
//		public static class SwitchDiv1 extends HtmlDiv {
//
//		}
//
//		@Switch(NextSwitcher.class)
//		@SetText("SwitchDiv2")
//		@Children(HtmlButton.class)
//		@BindAction(path = HtmlButton.class, value = NextAction.class)
//		@SetText(path = HtmlButton.class, value = "Next")
//		public static class SwitchDiv2 extends HtmlDiv {
//
//		}
//
//		@Switch(NextSwitcher.class)
//		@SetText("SwitchDiv3")
//		@Children(HtmlButton.class)
//		@BindAction(path = HtmlButton.class, value = NextAction.class)
//		@SetText(path = HtmlButton.class, value = "First")
//		public static class SwitchDiv3 extends HtmlDiv {
//
//		}
//
//		public static class NextSwitcher implements TagSwitcher {
//
//			@Override
//			public ObservableValue<Boolean> apply(Context context, Tag tag) {
//				Property<?> selectedClassProperty = tag.getProperty("selectedClass", context);
//				return Bindings.createBooleanBinding(() -> tag.getClass().equals(selectedClassProperty.getValue()), selectedClassProperty);
//			}
//
//		}
//	}

}
