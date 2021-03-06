package org.genericsystem.ir.app.gui.pages;

import org.genericsystem.ir.app.gui.pages.HomePage.DocClassDiv;
import org.genericsystem.ir.app.gui.pages.HomePage.GeneralActionsButtonsDiv;
import org.genericsystem.ir.app.gui.utils.ContextActionCustom.CALL_CLASSIFIER_PAGE;
import org.genericsystem.ir.app.gui.utils.ContextActionCustom.TEST;
import org.genericsystem.ir.app.gui.utils.PageSwitcher.HOME_PAGE;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.TagSwitcher.LOGGED_USER;
import org.genericsystem.reactor.context.TagSwitcher.NO_LOGGED_USER;
import org.genericsystem.reactor.gscomponents.AppHeader.AppTitleDiv;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH2;
import org.genericsystem.reactor.gscomponents.Monitor;

@Switch(HOME_PAGE.class)
@Children({ DocZonesEdit.class, Header.class, FlexDiv.class, FlexDiv.class, Monitor.class })
@Switch(path = FlexDiv.class, pos = 2, value = LOGGED_USER.class)
@Children(path = FlexDiv.class, pos = 2, value = { GeneralActionsButtonsDiv.class, DocClassDiv.class })
@Switch(path = FlexDiv.class, pos = 3, value = NO_LOGGED_USER.class)
@Children(path = FlexDiv.class, pos = 3, value = HtmlH2.class)
@SetText(path = { FlexDiv.class, HtmlH2.class }, pos = { 3, 0 }, value = "You must be identified in order to access this page.")
@SetText(path = { Header.class, AppTitleDiv.class, HtmlH1.class }, value = "GS-Watch interface")
public class HomePage extends FlexDiv {

	@FlexDirectionStyle(FlexDirection.ROW)
	@Children({ HtmlButton.class, HtmlButton.class })
	@SetText(path = HtmlButton.class, value = { "Classifier page", "Resume pending tasks" })
	@BindAction(path = HtmlButton.class, pos = 0, value = CALL_CLASSIFIER_PAGE.class)
	@BindAction(path = HtmlButton.class, pos = 1, value = TEST.class)
	@Attribute(path = HtmlButton.class, pos = 1, name = "disabled", value = "true")
	public static class GeneralActionsButtonsDiv extends FlexDiv {
		// TODO implement the action that will resume the waiting tasks
	}

	@Children(FlexDiv.class)
	// @Switch(path = FlexDiv.class, value = DOC_CLASS_NOT_EMPTY.class)
	// @ForEach(path = FlexDiv.class, value = DOC_CLASS_SELECTOR.class)
	@Children(path = FlexDiv.class, value = { FlexDiv.class, FlexDiv.class })
	@Children(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 1 }, value = HomePageTable.class)
	// @BindText(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 0 }, value = DOC_CLASS_LABEL.class)
	// @StyleClass(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 0 }, value = "doc-class-title")
	@Style(name = "width", value = "95%")
	@Style(name = "margin", value = "auto")
	@Style(path = FlexDiv.class, name = "margin", value = "0.5em 0")
	public static class DocClassDiv extends FlexDiv {

	}

}
