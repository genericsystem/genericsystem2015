package org.genericsystem.ir.app.gui.utils;

import java.io.File;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.ImgFilterFunction;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.defaults.tools.RxJavaHelpers;
import org.genericsystem.ir.DistributedVerticle;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.TagSwitcher;

import io.reactivex.Observable;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;

public class DocPropertiesSwitcher {

	private static final String ZONES_FILE_BASE_PATH = DistributedVerticle.BASE_PATH + "/classes/";

	public static class DOC_CLASS_EMPTY implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			return isDocClassEmpty(context, false);
		}
	}

	public static class DOC_CLASS_NOT_EMPTY implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			return isDocClassEmpty(context, true);
		}
	}

	public static class SUPERVISION_AVAILABLE implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			return isSupervisionAvailable(context, false);
		}
	}

	public static class SUPERVISION_NOT_AVAILABLE implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			return isSupervisionAvailable(context, true);
		}
	}

	public static class DOC_DEZONED implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			return isClassZoneFilePresent(context, false);
		}
	}

	public static class DOC_NOT_DEZONED implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			return isClassZoneFilePresent(context, true);
		}
	}

	public static class DOC_OCRD implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			return isDocOcrd(context, false);
		}
	}

	public static class DOC_NOT_OCRD implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			return isDocOcrd(context, true);
		}
	}

	public static class DOC_SUPERVISED implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			return isDocSupervised(context, false);
		}
	}

	public static class DOC_NOT_SUPERVISED implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			return isDocSupervised(context, true);
		}
	}

	public static Observable<Boolean> isDocClassEmpty(Context context, boolean reverse) {
		DocClassInstance currentDocClass = (DocClassInstance) context.getGeneric();
		Doc doc = context.getGeneric().getRoot().find(Doc.class);
		return currentDocClass.getHolders(doc).setOnChanged().map(set -> reverse ? !set.isEmpty() : set.isEmpty());
	}

	public static Observable<Boolean> isSupervisionAvailable(Context context, boolean reverse) {
		DocClassInstance currentDocClass = (DocClassInstance) context.getGeneric();
		Observable<Boolean> binding = getBooleanBinding(currentDocClass.getValue().toString());
		return reverse ? binding.map(bool -> !bool) : binding;
	}

	public static Observable<Boolean> isClassZoneFilePresent(Context context, boolean reverse) {
		DocInstance currentDoc = (DocInstance) context.getGeneric();
		DocClassInstance docClassInstance = currentDoc.getDocClass();
		Observable<Boolean> binding = getBooleanBinding(docClassInstance.getValue().toString());
		return reverse ? binding.map(bool -> !bool) : binding;
	}

	private static Observable<Boolean> getBooleanBinding(String docClass) {
		ObjectProperty<File> file = new SimpleObjectProperty<>(new File(ZONES_FILE_BASE_PATH + docClass + "/zones/zones.json"));
		return RxJavaHelpers.optionalValuesOf(file).map(opt -> opt.isPresent() && opt.get().exists());
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static Observable<Boolean> isDocOcrd(Context context, boolean reverse) {
		DocInstance currentDoc = (DocInstance) context.getGeneric();
		Root root = currentDoc.getRoot();
		ZoneText zoneText = root.find(ZoneText.class);
		ZoneGeneric zoneGeneric = root.find(ZoneGeneric.class);
		// The original image should ALWAYS exist
		ImgFilterInstance imgFilterInstance = ((ImgFilter) root.find(ImgFilter.class)).getImgFilter(ImgFilterFunction.ORIGINAL.getName());
		Observable<Boolean> obs = currentDoc.getHolders(zoneText).setOnChanged().map(zoneTextInstances -> {
			Snapshot<ZoneInstance> zoneInstances = (Snapshot) currentDoc.getDocClass().getHolders(zoneGeneric);
			// Consider the document as not OCR'd when the class was not de-zoned
			if (zoneInstances.isEmpty())
				return false;
			else { // Otherwise, return true only when all the zones have been processed
				return zoneInstances.stream().allMatch(zoneInstance -> {
					ZoneTextInstance zti = zoneText.getZoneText(currentDoc, zoneInstance, imgFilterInstance);
					// System.out.println(String.format("doc: %s | zone: %s | filter: %s | zti: %s", currentDoc, zoneInstance, imgFilterInstance, zti));
					return null != zti;
				});
			}
		});
		return reverse ? obs.map(bool -> !bool) : obs;
	}

	public static Observable<Boolean> isDocSupervised(Context context, boolean reverse) {
		// TODO: will a document be considered as not supervised if a field needs to be left empty?
		DocInstance currentDoc = (DocInstance) context.getGeneric();
		Root root = currentDoc.getRoot();
		ZoneText zoneText = root.find(ZoneText.class);
		Observable<Boolean> obs = currentDoc.getHolders(zoneText).filter(zt -> "reality".equals(((ZoneTextInstance) zt).getImgFilter().getValue()))
				.setOnChanged().map(zoneTextInstances -> !zoneTextInstances.isEmpty() && !zoneTextInstances.stream().anyMatch(g -> "".equals(g.getValue().toString())));
		return reverse ? obs.map(bool -> !bool) : obs;
	}
}
