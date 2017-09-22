package org.genericsystem.ir.app.gui.utils;

import org.apache.commons.lang3.StringEscapeUtils;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.Doc.DocTimestamp;
import org.genericsystem.cv.model.Doc.RefreshTimestamp;
import org.genericsystem.cv.model.Doc.RefreshTimestamp.RefreshTimestampInstance;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.MeanLevenshtein;
import org.genericsystem.cv.model.ModelTools;
import org.genericsystem.cv.model.Score.ScoreInstance;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.TextBinding;

import javafx.beans.binding.Bindings;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;

/**
 * This class contains all the {@link TextBinding} needed across the app.
 * 
 * @author Pierrik Lassalas
 */
public class TextBindingCustom {

	/*
	 * === DOC CLASS AND DOC ===
	 */

	public static class DOC_CLASS_LABEL implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			return new SimpleStringProperty("Doc class: " + String.valueOf(context.getGeneric().getValue()));
		}
	}

	/*
	 * === TIMESTAMP ===
	 */

	public static class LAST_DOC_UPDATE_LABEL implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			DocInstance currentDoc = (DocInstance) context.getGeneric();
			Root root = currentDoc.getRoot();
			DocTimestamp docTimestamp = root.find(DocTimestamp.class);
			SimpleObjectProperty<Generic> timeStamp = new SimpleObjectProperty<>(docTimestamp.getDocTimestamp(currentDoc));
			return Bindings.createStringBinding(() -> {
				return null == timeStamp || null == timeStamp.get() ? "n/a" : ModelTools.formatDate((Long) timeStamp.get().getValue());
			}, timeStamp);
		}
	}

	public static class LAST_REFRESH_UPDATE_LABEL implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			DocInstance currentDoc = (DocInstance) context.getGeneric();
			Root root = currentDoc.getRoot();
			RefreshTimestamp refreshTimestamp = root.find(RefreshTimestamp.class);
			return Bindings.createStringBinding(() -> {
				RefreshTimestampInstance timeStamp = refreshTimestamp.getRefreshTimestamp(currentDoc);
				return timeStamp == null ? "Last update: none" : "Last update: " + ModelTools.formatDate((Long) timeStamp.getValue());
			}, refreshTimestamp.getInstances().toObservableList());
		}
	}

	/*
	 * === ZONES ===
	 */

	public static class ZONE_LABEL implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			return new SimpleStringProperty("Zone " + ((ZoneTextInstance) context.getGeneric()).getZone());
		}
	}

	public static class ZONE_LABEL2 implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			ScoreInstance score = (ScoreInstance) context.getGeneric();
			return new SimpleStringProperty(String.valueOf(score.getZone().getValue()));
		}
	}

	public static class ZONE_TEXT implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			return new SimpleStringProperty(StringEscapeUtils.escapeHtml4(String.valueOf(context.getGeneric().getValue())));
		}
	}

	/*
	 * === OCR ===
	 */

	public static class OCR_TEXT implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			ZoneText zt = (ZoneText) context.getGeneric().getRoot().find(ZoneText.class);
			ImgFilterInstance ifi = (ImgFilterInstance) context.getGeneric();
			ZoneTextInstance zti = (ZoneTextInstance) context.getGenerics()[1];
			DocInstance doc = zti.getDoc();
			ZoneTextInstance text = zt.getZoneText(doc, zti.getZone(), ifi);
			return new SimpleStringProperty(text == null ? "" : StringEscapeUtils.escapeHtml4(String.valueOf(text.getValue())));
		}
	}

	/*
	 * === SCORES ===
	 */

	public static class SCORE_LABEL implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			ScoreInstance score = (ScoreInstance) context.getGeneric();
			SimpleObjectProperty<Generic> scoreProperty = new SimpleObjectProperty<>(score);
			return Bindings.createStringBinding(() -> String.valueOf(scoreProperty.get().getValue()), scoreProperty);
		}
	}

	public static class MEAN_LEV_LABEL implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			ScoreInstance score = (ScoreInstance) context.getGeneric();
			MeanLevenshtein meanLevenshtein = score.getRoot().find(MeanLevenshtein.class);
			SimpleObjectProperty<Generic> mlProperty = new SimpleObjectProperty<>(meanLevenshtein.getMeanLev(score));
			return Bindings.createStringBinding(() -> String.valueOf(mlProperty.get().getValue()), mlProperty);
		}
	}

	/*
	 * === IMG FILTERS ===
	 */

	public static class IMG_FILTER_LABEL implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			ScoreInstance score = (ScoreInstance) context.getGeneric();
			ImgFilterInstance imgFilterInstance = score.getImgFilter();
			return new SimpleStringProperty(String.valueOf(imgFilterInstance.getValue()));
		}
	}

}
