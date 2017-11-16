package org.genericsystem.ir.app.gui.utils;

import java.io.Serializable;

import org.apache.commons.lang3.StringEscapeUtils;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.newmodel.SimpleModel.ConsolidatedType.ConsolidatedInstance;
import org.genericsystem.cv.newmodel.SimpleModel.ImgRefreshTimestampType;
import org.genericsystem.cv.newmodel.SimpleModel.ImgType.ImgInstance;
import org.genericsystem.cv.newmodel.SimpleModel.SupervisedType.SupervisedInstance;
import org.genericsystem.cv.newmodel.SimpleModel.ZoneType.ZoneInstance;
import org.genericsystem.cv.utils.ModelTools;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.TextBinding;

import io.reactivex.Observable;
import javafx.beans.property.SimpleObjectProperty;

/**
 * This class contains all the {@link TextBinding} needed across the app.
 */
public class TextBindingCustom {

	/*
	 * === TIMESTAMP ===
	 */

	public static class LAST_DOC_UPDATE_LABEL implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			ImgInstance currentImg = (ImgInstance) context.getGeneric();
			SimpleObjectProperty<Generic> timeStamp = new SimpleObjectProperty<>(currentImg.getImgTimestamp());
			return Observable.just(null == timeStamp || null == timeStamp.get() ? "n/a" : ModelTools.formatDate((Long) timeStamp.get().getValue()));
		}
	}

	public static class LAST_REFRESH_UPDATE_LABEL implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			ImgInstance currentImg = (ImgInstance) context.getGeneric();
			Root root = currentImg.getRoot();
			ImgRefreshTimestampType refreshTimestamp = root.find(ImgRefreshTimestampType.class);
			System.out.println(refreshTimestamp.info());
			System.out.println(refreshTimestamp.getInstances(currentImg));
			return refreshTimestamp.getInstances(currentImg).getAdds().map(timeStamp -> formatTimeStamp(timeStamp)).startWith(formatTimeStamp(currentImg.getImgRefreshTimestamp()));
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
			return Observable.just("Zone " + ((ZoneInstance) context.getGeneric()).getZoneNum().getValue());
		}
	}

	public static class ZONE_TEXT implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			ConsolidatedInstance text = ((ZoneInstance) context.getGeneric()).getConsolidated();
			return Observable.just(getEscapedText(text.getValue()));
		}
	}

	public static class ZONE_TEXT_REALITY implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			SupervisedInstance text = ((SupervisedInstance) context.getGeneric());
			// XXX: use the reality text only
			return Observable.just(getEscapedText(text.getValue()));
		}
	}

	/**
	 * Escape the HTML characters in a Serializable.
	 * 
	 * @param value - the serializable value of the generic
	 * @return a HTML safe string
	 */
	private static String getEscapedText(Serializable value) {
		return StringEscapeUtils.escapeHtml4(String.valueOf(value));
	}

}
