package org.genericsystem.watch.gui.pages;

import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.MeanLevenshtein;
import org.genericsystem.cv.model.MeanLevenshtein.MeanLevenshteinInstance;
import org.genericsystem.cv.model.Score.ScoreInstance;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.watch.gui.pages.StatisticsTable.ContentRow;
import org.genericsystem.watch.gui.pages.StatisticsTable.HeaderRow;
import org.genericsystem.watch.gui.utils.ObservableListExtractorCustom.SCORE_SELECTOR;
import org.genericsystem.watch.gui.utils.ObservableListExtractorCustom.ZONE_SELECTOR;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;

@Children({ HeaderRow.class, FlexDiv.class })
@Children(path = FlexDiv.class, pos = 1, value = ContentRow.class)
@ForEach(path = FlexDiv.class, pos = 1, value = ZONE_SELECTOR.class)
@FlexDirectionStyle(path = FlexDiv.class, pos = 1, value = FlexDirection.COLUMN)
public class StatisticsTable extends FlexDiv implements SelectionDefaults {

	@Children({ HtmlLabel.class, HtmlLabel.class, HtmlLabel.class, HtmlLabel.class })
	@FlexDirectionStyle(FlexDirection.ROW)
	@Attribute(path = HtmlLabel.class, name = "name", value = "title")
	@Style(name = "margin", value = "0.5em")
	@Style(path = HtmlLabel.class, name = "justify-content", value = "center")
	@Style(path = HtmlLabel.class, name = "align-items", value = "center")
	@Style(path = HtmlLabel.class, name = "flex", value = "1")
	@Style(path = HtmlLabel.class, name = "text-align", value = "center")
	@Style(path = HtmlLabel.class, name = "font-weight", value = "bold")
	@SetText(path = HtmlLabel.class, value = { "Zone", "Img Filter", "Score (probability)", "Mean Levenshtein Distance" })
	public static class HeaderRow extends FlexDiv {

	}

	@FlexDirectionStyle(FlexDirection.ROW)
	@Style(path = FlexDiv.class, name = "justify-content", value = "center")
	@Style(path = FlexDiv.class, name = "align-items", value = "center")
	@Style(path = FlexDiv.class, name = "flex", value = "1")
	@ForEach(SCORE_SELECTOR.class)
	@FlexDirectionStyle(path = FlexDiv.class, value = FlexDirection.COLUMN)
	@Children({ FlexDiv.class, FlexDiv.class, FlexDiv.class, FlexDiv.class })
	@BindText(path = FlexDiv.class, pos = 0, value = ZONE_LABEL.class)
	@BindText(path = FlexDiv.class, pos = 1, value = IMG_FILTER_LABEL.class)
	@BindText(path = FlexDiv.class, pos = 2, value = SCORE_LABEL.class)
	@BindText(path = FlexDiv.class, pos = 3, value = MEAN_LEV_LABEL.class)
	public static class ContentRow extends FlexDiv {

	}

	public static class ZONE_LABEL implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			ScoreInstance score = (ScoreInstance) context.getGeneric();
			return new SimpleStringProperty(score.getZone().getValue().toString());
		}
	}

	public static class SCORE_LABEL implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			ScoreInstance score = (ScoreInstance) context.getGeneric();
			return new SimpleStringProperty(score.getValue().toString());
		}
	}

	public static class IMG_FILTER_LABEL implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			ScoreInstance score = (ScoreInstance) context.getGeneric();
			ImgFilterInstance imgFilterInstance = score.getImgFilter();
			return new SimpleStringProperty(imgFilterInstance.getValue().toString());
		}
	}

	public static class MEAN_LEV_LABEL implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			ScoreInstance score = (ScoreInstance) context.getGeneric();
			MeanLevenshtein meanLevenshtein = score.getRoot().find(MeanLevenshtein.class);
			MeanLevenshteinInstance mlvInstance = meanLevenshtein.getMeanLev(score);
			return new SimpleStringProperty(mlvInstance.getValue().toString());
		}
	}

}
