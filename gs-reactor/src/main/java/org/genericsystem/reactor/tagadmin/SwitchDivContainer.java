package org.genericsystem.reactor.tagadmin;

import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Step;
import org.genericsystem.reactor.annotations.Stepper;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.extended.ExtendedRootTag.GTag;
import org.genericsystem.reactor.extended.ExtendedRootTag.TagType;
import org.genericsystem.reactor.gscomponents.Controller;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.tagadmin.SwitchDivContainer.SwitchDiv1;
import org.genericsystem.reactor.tagadmin.SwitchDivContainer.SwitchDiv2;
import org.genericsystem.reactor.tagadmin.SwitchDivContainer.SwitchDiv3;
import org.genericsystem.reactor.tagadmin.SwitchDivContainer.SwitchDiv4;
import org.genericsystem.reactor.tagadmin.SwitchDivContainer.SwitchDiv5;

@DependsOnModel({ TagType.class, GTag.class })
@Stepper(first = SwitchDiv1.class)
@Children({ SwitchDiv1.class, SwitchDiv2.class, SwitchDiv3.class, SwitchDiv4.class, SwitchDiv5.class })
public class SwitchDivContainer extends HtmlDiv {

	@Children({ HtmlButton.class, HtmlLabel.class, HtmlButton.class })
	@BindAction(path = HtmlButton.class, pos = { 0 }, value = Controller.PrevAction.class)
	@BindAction(path = HtmlButton.class, pos = { 1 }, value = Controller.NextAction.class)
	@BindText(path = HtmlButton.class, pos = { 0 }, value = Controller.PrevTextBinding.class)
	@BindText(path = HtmlLabel.class, value = Controller.CountTextBinding.class)
	@BindText(path = HtmlButton.class, pos = { 1 }, value = Controller.NextTextBinding.class)
	@Switch(path = HtmlButton.class, pos = { 0 }, value = Controller.PrevSwitcher.class)
	@Switch(path = HtmlLabel.class, value = Controller.CountTextSwitcher.class)
	@Switch(path = HtmlButton.class, pos = { 1 }, value = Controller.NextSwitcher.class)
	public static class StepDiv extends HtmlDiv {

	}

	@Step(next = SwitchDiv2.class)
	@SetText("SwitchDiv1")
	public static class SwitchDiv1 extends StepDiv {

	}

	@Step(next = SwitchDiv3.class)
	@SetText("SwitchDiv2")
	public static class SwitchDiv2 extends StepDiv {

	}

	@Step(next = SwitchDiv4.class)
	@SetText("SwitchDiv3")
	public static class SwitchDiv3 extends StepDiv {

	}

	@Step(next = SwitchDiv5.class)
	@SetText("SwitchDiv4")
	@ForEach(ObservableListExtractor.INSTANCES.class)
	public static class SwitchDiv4 extends StepDiv {

	}

	@Step(next = SwitchDiv5.class)
	@SetText("SwitchDiv5")
	@ForEach(ObservableListExtractor.INSTANCES.class)
	public static class SwitchDiv5 extends StepDiv {

	}

}
