package org.genericsystem.ir.app.gui.utils;

import org.apache.commons.lang3.StringEscapeUtils;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.Doc.DocTimestamp;
import org.genericsystem.cv.model.Doc.RefreshTimestamp;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.MeanLevenshtein;
import org.genericsystem.cv.model.ModelTools;
import org.genericsystem.cv.model.Score.ScoreInstance;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.defaults.tools.RxJavaHelpers;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.TextBinding;

import io.reactivex.Observable;
import javafx.beans.property.SimpleObjectProperty;

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
		public Observable<String> apply(Context context, Tag tag) {
			return Observable.just("Doc class: " + String.valueOf(context.getGeneric().getValue()));
		}
	}

	/*
	 * === TIMESTAMP ===
	 */

	public static class LAST_DOC_UPDATE_LABEL implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			DocInstance currentDoc = (DocInstance) context.getGeneric();
			Root root = currentDoc.getRoot();
			DocTimestamp docTimestamp = root.find(DocTimestamp.class);
			SimpleObjectProperty<Generic> timeStamp = new SimpleObjectProperty<>(docTimestamp.getDocTimestamp(currentDoc));
			return Observable.just(null == timeStamp || null == timeStamp.get() ? "n/a" : ModelTools.formatDate((Long) timeStamp.get().getValue()));
		}
	}

	public static class LAST_REFRESH_UPDATE_LABEL implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			DocInstance currentDoc = (DocInstance) context.getGeneric();
			Root root = currentDoc.getRoot();
			RefreshTimestamp refreshTimestamp = root.find(RefreshTimestamp.class);
			return Observable.concat(Observable.just(formatTimeStamp(refreshTimestamp.getRefreshTimestamp(currentDoc))),
					refreshTimestamp.getInstances(currentDoc).getAddsObservable().map(timeStamp -> formatTimeStamp(timeStamp)));
		}

		private String formatTimeStamp(Generic timeStamp) {
			return timeStamp == null ? "Last update: none" : "Last update: " + ModelTools.formatDate((Long) timeStamp.getValue());
		}
	}

	/*
	 * === ZONES ===
	 */

	public static class ZONE_LABEL implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			return Observable.just("Zone " + ((ZoneTextInstance) context.getGeneric()).getZone());
		}
	}

	public static class ZONE_LABEL2 implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			ScoreInstance score = (ScoreInstance) context.getGeneric();
			return Observable.just(String.valueOf(score.getZone().getValue()));
		}
	}

	public static class ZONE_TEXT implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			return Observable.just(StringEscapeUtils.escapeHtml4(String.valueOf(context.getGeneric().getValue())));
		}
	}

	/*
	 * === OCR ===
	 */

	public static class OCR_TEXT implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			ZoneText zt = (ZoneText) context.getGeneric().getRoot().find(ZoneText.class);
			ImgFilterInstance ifi = (ImgFilterInstance) context.getGeneric();
			ZoneTextInstance zti = (ZoneTextInstance) context.getGenerics()[1];
			DocInstance doc = zti.getDoc();
			ZoneTextInstance text = zt.getZoneText(doc, zti.getZone(), ifi);
			return Observable.just(text == null ? "" : StringEscapeUtils.escapeHtml4(String.valueOf(text.getValue())));
		}
	}

	/*
	 * === SCORES ===
	 */

	public static class SCORE_LABEL implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			ScoreInstance score = (ScoreInstance) context.getGeneric();
			SimpleObjectProperty<Generic> scoreProperty = new SimpleObjectProperty<>(score);
			return RxJavaHelpers.valuesOf(scoreProperty).map(scoreInstance -> String.valueOf(scoreInstance.getValue()));
		}
	}

	public static class MEAN_LEV_LABEL implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			ScoreInstance score = (ScoreInstance) context.getGeneric();
			MeanLevenshtein meanLevenshtein = score.getRoot().find(MeanLevenshtein.class);
			SimpleObjectProperty<Generic> mlProperty = new SimpleObjectProperty<>(meanLevenshtein.getMeanLev(score));
			return RxJavaHelpers.valuesOf(mlProperty).map(ml -> String.valueOf(ml.getValue()));
		}
	}

	/*
	 * === IMG FILTERS ===
	 */

	public static class IMG_FILTER_LABEL implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			ScoreInstance score = (ScoreInstance) context.getGeneric();
			ImgFilterInstance imgFilterInstance = score.getImgFilter();
			return Observable.just(String.valueOf(imgFilterInstance.getValue()));
		}
	}

}
