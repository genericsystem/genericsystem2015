package org.genericsystem.quiz.app;

import org.genericsystem.common.Root;
import org.genericsystem.quiz.app.SwitchApp.SwitchAppScript;
import org.genericsystem.quiz.app.SwitchApp.SwitchDiv1;
import org.genericsystem.quiz.app.SwitchApp.SwitchDiv2;
import org.genericsystem.quiz.app.SwitchApp.SwitchDiv3;
import org.genericsystem.quiz.app.SwitchApp.SwitchDiv4;
import org.genericsystem.quiz.app.SwitchApp.SwitchDiv5;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.Step;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.annotations.Switcher;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.Script;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.gscomponents.Controller;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.RootTagImpl;

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

	@Switch(Controller.MainSwitcher.class)
	@Children({ HtmlButton.class, HtmlLabel.class, HtmlButton.class })
	@BindAction(path = HtmlButton.class, pos = { 0 }, value = Controller.PrevAction.class)
	@BindAction(path = HtmlButton.class, pos = { 1 }, value = Controller.NextAction.class)
	@BindText(path = HtmlButton.class, pos = { 0 }, value = Controller.PrevTextBinding.class)
	@BindText(path = HtmlLabel.class, value = Controller.CountTextBinding.class)
	@BindText(path = HtmlButton.class, pos = { 1 }, value = Controller.NextTextBinding.class)
	@Switch(path = HtmlButton.class, pos = { 0 }, value = Controller.PrevSwitcher.class)
	@Switch(path = HtmlButton.class, pos = { 1 }, value = Controller.NextSwitcher.class)
	public static class StepDiv extends HtmlDiv {

	}

	@Step(nextClass = SwitchDiv2.class)
	public static class SwitchDiv1 extends StepDiv {

	}

	@Step(nextClass = SwitchDiv3.class)
	public static class SwitchDiv2 extends StepDiv {

	}

	@Step(nextClass = SwitchDiv4.class)
	public static class SwitchDiv3 extends StepDiv {

	}

	@Step(nextClass = SwitchDiv5.class)
	@ForEach(ObservableListExtractor.COMPONENTS.class)
	public static class SwitchDiv4 extends StepDiv {

	}

	@Step(nextClass = SwitchDiv5.class)
	@ForEach(ObservableListExtractor.INSTANCES.class)
	public static class SwitchDiv5 extends StepDiv {

	}

}
