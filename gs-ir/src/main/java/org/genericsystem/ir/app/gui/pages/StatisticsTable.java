// package org.genericsystem.ir.app.gui.pages;
//
// import org.genericsystem.ir.app.gui.pages.StatisticsTable.ContentRow;
// import org.genericsystem.ir.app.gui.pages.StatisticsTable.HeaderRow;
// import org.genericsystem.ir.app.gui.utils.ObservableListExtractorCustom.SCORE_SELECTOR;
// import org.genericsystem.ir.app.gui.utils.ObservableListExtractorCustom.ZONE_SELECTOR;
// import org.genericsystem.ir.app.gui.utils.TextBindingCustom.IMG_FILTER_LABEL;
// import org.genericsystem.ir.app.gui.utils.TextBindingCustom.MEAN_LEV_LABEL;
// import org.genericsystem.ir.app.gui.utils.TextBindingCustom.SCORE_LABEL;
// import org.genericsystem.ir.app.gui.utils.TextBindingCustom.ZONE_LABEL2;
// import org.genericsystem.reactor.annotations.Attribute;
// import org.genericsystem.reactor.annotations.BindText;
// import org.genericsystem.reactor.annotations.Children;
// import org.genericsystem.reactor.annotations.ForEach;
// import org.genericsystem.reactor.annotations.SetText;
// import org.genericsystem.reactor.annotations.Style;
// import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
// import org.genericsystem.reactor.annotations.StyleClass;
// import org.genericsystem.reactor.contextproperties.SelectionDefaults;
// import org.genericsystem.reactor.gscomponents.FlexDirection;
// import org.genericsystem.reactor.gscomponents.FlexDiv;
// import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
//
// @Children({ HeaderRow.class, FlexDiv.class })
// @Children(path = FlexDiv.class, pos = 1, value = ContentRow.class)
// @ForEach(path = FlexDiv.class, pos = 1, value = ZONE_SELECTOR.class)
// @StyleClass(path = FlexDiv.class, pos = 1, value = "alternate-rows")
// @FlexDirectionStyle(path = FlexDiv.class, pos = 1, value = FlexDirection.COLUMN)
// public class StatisticsTable extends FlexDiv implements SelectionDefaults {
//
// @Children({ HtmlLabel.class, HtmlLabel.class, HtmlLabel.class, HtmlLabel.class })
// @FlexDirectionStyle(FlexDirection.ROW)
// @Attribute(path = HtmlLabel.class, name = "name", value = "title")
// @StyleClass("statistics-header-row")
// @Style(path = HtmlLabel.class, name = "justify-content", value = "center")
// @Style(path = HtmlLabel.class, name = "align-items", value = "center")
// @Style(path = HtmlLabel.class, name = "flex", value = "1")
// @Style(path = HtmlLabel.class, name = "text-align", value = "center")
// @Style(path = HtmlLabel.class, name = "font-weight", value = "bold")
// @SetText(path = HtmlLabel.class, value = { "Zone", "Img Filter", "Score (probability)", "Mean Levenshtein Distance" })
// public static class HeaderRow extends FlexDiv {
//
// }
//
// @FlexDirectionStyle(FlexDirection.ROW)
// @Style(path = FlexDiv.class, name = "justify-content", value = "center")
// @Style(path = FlexDiv.class, name = "align-items", value = "center")
// @Style(path = FlexDiv.class, name = "flex", value = "1")
// @ForEach(SCORE_SELECTOR.class)
// @FlexDirectionStyle(path = FlexDiv.class, value = FlexDirection.COLUMN)
// @Children({ FlexDiv.class, FlexDiv.class, FlexDiv.class, FlexDiv.class })
// @BindText(path = FlexDiv.class, pos = 0, value = ZONE_LABEL2.class)
// @BindText(path = FlexDiv.class, pos = 1, value = IMG_FILTER_LABEL.class)
// @BindText(path = FlexDiv.class, pos = 2, value = SCORE_LABEL.class)
// @BindText(path = FlexDiv.class, pos = 3, value = MEAN_LEV_LABEL.class)
// public static class ContentRow extends FlexDiv {
//
// }
//
// }
