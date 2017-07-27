package org.genericsystem.watch.gui.pages;

import org.genericsystem.cv.model.MeanLevenshtein;
import org.genericsystem.cv.model.Score;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.gscomponents.AppHeader;
import org.genericsystem.reactor.gscomponents.AppHeader.AppTitleDiv;
import org.genericsystem.reactor.gscomponents.AppHeader.Logo;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.watch.gui.pages.FiltersStatisticsPage.DocClassStatisticsDiv;
import org.genericsystem.watch.gui.pages.FiltersStatisticsPage.GeneralButtonsDiv;
import org.genericsystem.watch.gui.utils.ContextActionCustom.CALL_HOME_PAGE;
import org.genericsystem.watch.gui.utils.ContextActionCustom.COMPUTE_STATS;
import org.genericsystem.watch.gui.utils.ContextActionCustom.COMPUTE_STATS_STRICT;
import org.genericsystem.watch.gui.utils.DocPropertiesSwitcher.SUPERVISION_AVAILABLE;
import org.genericsystem.watch.gui.utils.ObservableListExtractorCustom.DOC_CLASS_SELECTOR;
import org.genericsystem.watch.gui.utils.PageSwitcher.FILTERS_STATISTICS;

/**
 * This class provides a gs-reactor application to visualize and update the {@link Score} and {@link MeanLevenshtein} distances for each filters.
 * 
 * @author Pierrik Lassalas
 */
// TODO: redesign the interface (smaller, add foreach loops)
@Switch(FILTERS_STATISTICS.class)
@Style(name = "background-color", value = "#ffffff")
@Children({ AppHeader.class, FlexDiv.class })
@Children(path = FlexDiv.class, pos = 1, value = { GeneralButtonsDiv.class, DocClassStatisticsDiv.class })
@Style(path = AppHeader.class, name = "background-color", value = "#00afeb")
@Children(path = AppHeader.class, value = { Logo.class, AppTitleDiv.class })
@SetText(path = { AppHeader.class, AppTitleDiv.class, HtmlH1.class }, value = "Global OCR accuracy per zone")
public class FiltersStatisticsPage extends FlexDiv {

	@FlexDirectionStyle(FlexDirection.ROW)
	@Children(HtmlButton.class)
	@SetText(path = HtmlButton.class, value = "Home Page")
	@BindAction(path = HtmlButton.class, pos = 0, value = CALL_HOME_PAGE.class)
	public static class GeneralButtonsDiv extends FlexDiv {

	}

	@Children({ FlexDiv.class, FlexDiv.class })
	@Children(path = FlexDiv.class, pos = 1, value = StatisticsTable.class)
	@Children(path = FlexDiv.class, pos = 0, value = { FlexDiv.class, FlexDiv.class })
	@Children(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 1 }, value = ButtonDiv.class)
	@BindText(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 0 })
	@FlexDirectionStyle(path = FlexDiv.class, pos = 0, value = FlexDirection.ROW)
	@Style(path = FlexDiv.class, pos = 0, name = "justify-content", value = "center")
	@Style(path = FlexDiv.class, pos = 0, name = "align-items", value = "center")
	@Style(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 0 }, name = "flex", value = "1 1 auto")
	@Style(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 1 }, name = "flex", value = "0 1 auto")
	@StyleClass(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 0 }, value = "doc-class-title")
	@Style(name = "width", value = "90%")
	@Style(name = "margin", value = "auto")
	@Switch(SUPERVISION_AVAILABLE.class)
	@ForEach(DOC_CLASS_SELECTOR.class)
	public static class DocClassStatisticsDiv extends FlexDiv {

	}

	@Children({ ComputeStatsButton.class, ComputeStatsStrictButton.class })
	@FlexDirectionStyle(FlexDirection.ROW)
	public static class ButtonDiv extends FlexDiv {

	}

	@SetText("Compute statistics")
	@BindAction({ COMPUTE_STATS.class })
	public static class ComputeStatsButton extends HtmlButton {

	}

	@SetText("Compute statistics (strict mode)")
	@BindAction({ COMPUTE_STATS_STRICT.class })
	public static class ComputeStatsStrictButton extends HtmlButton {

	}

}