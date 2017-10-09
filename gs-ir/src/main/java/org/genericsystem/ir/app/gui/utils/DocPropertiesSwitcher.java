package org.genericsystem.ir.app.gui.utils;

import org.genericsystem.cv.classifier.newmodel.SimpleModel.DocClassType.DocClassInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgType.ImgInstance;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.TagSwitcher;

import io.reactivex.Observable;

@SuppressWarnings({ "unchecked", "rawtypes" })
public class DocPropertiesSwitcher {

	// private static final String ZONES_FILE_BASE_PATH = DistributedVerticle.BASE_PATH + "/classes/";

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

	// public static class SUPERVISION_AVAILABLE implements TagSwitcher {
	// @Override
	// public Observable<Boolean> apply(Context context, Tag tag) {
	// return isSupervisionAvailable(context, false);
	// }
	// }
	//
	// public static class SUPERVISION_NOT_AVAILABLE implements TagSwitcher {
	// @Override
	// public Observable<Boolean> apply(Context context, Tag tag) {
	// return isSupervisionAvailable(context, true);
	// }
	// }

	public static class DOC_DEZONED implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			return isImgDezoned(context, false);
		}
	}

	public static class DOC_NOT_DEZONED implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			return isImgDezoned(context, true);
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

	// public static class DOC_SUPERVISED implements TagSwitcher {
	// @Override
	// public Observable<Boolean> apply(Context context, Tag tag) {
	// return isDocSupervised(context, false);
	// }
	// }
	//
	// public static class DOC_NOT_SUPERVISED implements TagSwitcher {
	// @Override
	// public Observable<Boolean> apply(Context context, Tag tag) {
	// return isDocSupervised(context, true);
	// }
	// }

	public static Observable<Boolean> isDocClassEmpty(Context context, boolean reverse) {
		DocClassInstance currentDocClass = (DocClassInstance) context.getGeneric();
		Observable<Boolean> binding = currentDocClass.getAllDocInstances().setOnChanged().map(docs -> docs.isEmpty());
		return reverse ? binding.map(bool -> !bool) : binding;
	}

	// public static Observable<Boolean> isSupervisionAvailable(Context context, boolean reverse) {
	// DocClassInstance currentDocClass = (DocClassInstance) context.getGeneric();
	// Observable<Boolean> binding = getBooleanBinding(currentDocClass.getValue().toString());
	// return reverse ? binding.map(bool -> !bool) : binding;
	// }

	public static Observable<Boolean> isImgDezoned(Context context, boolean reverse) {
		ImgInstance imgInstance = (ImgInstance) context.getGeneric();
		Observable<Boolean> binding = getBooleanBinding(imgInstance);
		return reverse ? binding.map(bool -> !bool) : binding;
	}

	private static Observable<Boolean> getBooleanBinding(ImgInstance imgInstance) {
		return imgInstance.getZoneInstances().setOnChanged().map(zones -> !zones.isEmpty());
	}

	public static Observable<Boolean> isDocOcrd(Context context, boolean reverse) {
		ImgInstance currentImg = (ImgInstance) context.getGeneric();
		Observable<Boolean> obs = currentImg.getConsolidatedZoneInstances().setOnChanged().map(zone -> !zone.isEmpty());
		return reverse ? obs.map(bool -> !bool) : obs;
	}

	// public static Observable<Boolean> isDocSupervised(Context context, boolean reverse) {
	// // TODO: will a document be considered as not supervised if a field needs to be left empty?
	// DocInstance currentDoc = (DocInstance) context.getGeneric();
	// Root root = currentDoc.getRoot();
	// ZoneText zoneText = root.find(ZoneText.class);
	// Observable<Boolean> obs = currentDoc.getHolders(zoneText).filter(zt -> "reality".equals(((ZoneTextInstance) zt).getImgFilter().getValue())).setOnChanged()
	// .map(zoneTextInstances -> !zoneTextInstances.isEmpty() && !zoneTextInstances.stream().anyMatch(g -> "".equals(g.getValue().toString())));
	// return reverse ? obs.map(bool -> !bool) : obs;
	// }
}
