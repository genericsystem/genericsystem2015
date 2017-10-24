package org.genericsystem.ir.app.gui.pages;

import org.genericsystem.cv.model.MeanLevenshtein;
import org.genericsystem.cv.model.Score;
import org.genericsystem.ir.app.gui.pages.ClassifierPage.DocClassDiv;
import org.genericsystem.ir.app.gui.pages.ClassifierPage.GeneralButtonsDiv;
import org.genericsystem.ir.app.gui.utils.ContextActionCustom.CALL_HOME_PAGE;
import org.genericsystem.ir.app.gui.utils.DocPropertiesSwitcher.DOC_CLASS_NOT_EMPTY;
import org.genericsystem.ir.app.gui.utils.ObservableListExtractorCustom.DOC_CLASS_SELECTOR;
import org.genericsystem.ir.app.gui.utils.PageSwitcher.CLASSIFIER_PAGE;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.gscomponents.AppHeader.AppTitleDiv;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;

/**
 * This class provides a gs-reactor application to visualize and update the {@link Score} and {@link MeanLevenshtein} distances for each filters.
 * 
 * @author Pierrik Lassalas
 */

@Switch(CLASSIFIER_PAGE.class)
@Style(name = "background-color", value = "#ffffff")
@Children({ Header.class, FlexDiv.class })
@Children(path = FlexDiv.class, pos = 1, value = { GeneralButtonsDiv.class, DocClassDiv.class })
@SetText(path = { Header.class, AppTitleDiv.class, HtmlH1.class }, value = "Document classification")
public class ClassifierPage extends FlexDiv {

	@FlexDirectionStyle(FlexDirection.ROW)
	@Children(HtmlButton.class)
	@SetText(path = HtmlButton.class, value = "Home Page")
	@BindAction(path = HtmlButton.class, pos = 0, value = CALL_HOME_PAGE.class)
	public static class GeneralButtonsDiv extends FlexDiv {

	}

	@Switch(path = FlexDiv.class, value = DOC_CLASS_NOT_EMPTY.class)
	@Children(FlexDiv.class)
	@Children(path = FlexDiv.class, value = { FlexDiv.class, FlexDiv.class })
	@Children(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 0 }, value = { FlexDiv.class, FlexDiv.class })
	@Children(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 1 }, value = ClassifierTable.class)
	@BindText(path = { FlexDiv.class, FlexDiv.class, FlexDiv.class }, pos = { 0, 0, 0 })
	@FlexDirectionStyle(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 0 }, value = FlexDirection.ROW)
	@Style(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 0 }, name = "justify-content", value = "center")
	@Style(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 0 }, name = "align-items", value = "center")
	@Style(path = { FlexDiv.class, FlexDiv.class, FlexDiv.class }, pos = { 0, 0, 0 }, name = "flex", value = "1 1 auto")
	@Style(path = { FlexDiv.class, FlexDiv.class, FlexDiv.class }, pos = { 0, 0, 1 }, name = "flex", value = "0 1 auto")
	@StyleClass(path = { FlexDiv.class, FlexDiv.class, FlexDiv.class }, pos = { 0, 0, 0 }, value = "doc-class-title")
	@Style(name = "width", value = "95%")
	@Style(name = "margin", value = "auto")
	@Style(path = FlexDiv.class, name = "margin", value = "0.5em 0")
	@ForEach(path = FlexDiv.class, value = DOC_CLASS_SELECTOR.class)
	public static class DocClassDiv extends FlexDiv {

	}

}