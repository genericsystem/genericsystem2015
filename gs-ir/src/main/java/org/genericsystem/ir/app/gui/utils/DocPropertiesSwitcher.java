package org.genericsystem.ir.app.gui.utils;

import java.io.File;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
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
import org.genericsystem.ir.DistributedVerticle;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.TagSwitcher;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.BooleanBinding;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

public class DocPropertiesSwitcher {

	private static final String ZONES_FILE_BASE_PATH = DistributedVerticle.BASE_PATH + "/classes/";

	public static class DOC_CLASS_EMPTY implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isDocClassEmpty(context, false);
		}
	}

	public static class DOC_CLASS_NOT_EMPTY implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isDocClassEmpty(context, true);
		}
	}

	public static class SUPERVISION_AVAILABLE implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isSupervisionAvailable(context, false);
		}
	}

	public static class SUPERVISION_NOT_AVAILABLE implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isSupervisionAvailable(context, true);
		}
	}

	public static class DOC_DEZONED implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isClassZoneFilePresent(context, false);
		}
	}

	public static class DOC_NOT_DEZONED implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isClassZoneFilePresent(context, true);
		}
	}

	public static class DOC_OCRD implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isDocOcrd(context, false);
		}
	}

	public static class DOC_NOT_OCRD implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isDocOcrd(context, true);
		}
	}

	public static class DOC_SUPERVISED implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isDocSupervised(context, false);
		}
	}

	public static class DOC_NOT_SUPERVISED implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isDocSupervised(context, true);
		}
	}

	public static ObservableValue<Boolean> isDocClassEmpty(Context context, boolean reverse) {
		DocClassInstance currentDocClass = (DocClassInstance) context.getGeneric();
		Doc doc = context.getGeneric().getRoot().find(Doc.class);
		ObservableList<Generic> docInstances = currentDocClass.getHolders(doc).toObservableList();
		BooleanBinding binding = Bindings.createBooleanBinding(() -> {
			return null == docInstances || docInstances.isEmpty();
		}, docInstances);
		return reverse ? binding.not() : binding;
	}

	public static ObservableValue<Boolean> isSupervisionAvailable(Context context, boolean reverse) {
		DocClassInstance currentDocClass = (DocClassInstance) context.getGeneric();
		BooleanBinding binding = getBooleanBinding(currentDocClass.getValue().toString());
		return reverse ? binding.not() : binding;
	}

	public static ObservableValue<Boolean> isClassZoneFilePresent(Context context, boolean reverse) {
		DocInstance currentDoc = (DocInstance) context.getGeneric();
		DocClassInstance docClassInstance = currentDoc.getDocClass();
		BooleanBinding binding = getBooleanBinding(docClassInstance.getValue().toString());
		return reverse ? binding.not() : binding;
	}

	private static BooleanBinding getBooleanBinding(String docClass) {
		ObjectProperty<File> file = new SimpleObjectProperty<>(new File(ZONES_FILE_BASE_PATH + docClass + "/zones/zones.json"));
		BooleanBinding binding = Bindings.createBooleanBinding(() -> {
			return null != file.get() && file.get().exists();
		}, file);
		return binding;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public static ObservableValue<Boolean> isDocOcrd(Context context, boolean reverse) {
		// TODO: modify the algorithm so each zone needs to be processed before returning true
		DocInstance currentDoc = (DocInstance) context.getGeneric();
		Root root = currentDoc.getRoot();
		ZoneText zoneText = root.find(ZoneText.class);
		ZoneGeneric zoneGeneric = root.find(ZoneGeneric.class);
		// The original image should ALWAYS exist
		ImgFilterInstance imgFilterInstance = ((ImgFilter) root.find(ImgFilter.class)).getImgFilter(ImgFilterFunction.ORIGINAL.getName());
		ObservableList<ZoneTextInstance> zoneTextInstances = (ObservableList) currentDoc.getHolders(zoneText).toObservableList();
		BooleanBinding binding = Bindings.createBooleanBinding(() -> {
			Snapshot<ZoneInstance> zoneInstances = (Snapshot) zoneGeneric.getInstances();
			// Consider the document as not OCR'd when the class was not de-zoned
			if (zoneInstances.isEmpty())
				return false;
			else { // Otherwise, return true only when all the zones have been processed
				return zoneInstances.stream().allMatch(zoneInstance -> {
					ZoneTextInstance zti = zoneText.getZoneText(currentDoc, zoneInstance, imgFilterInstance);
					return null != zti;
				});
			}
		}, zoneTextInstances);
		return reverse ? binding.not() : binding;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static ObservableValue<Boolean> isDocSupervised(Context context, boolean reverse) {
		// TODO: will a document be considered as not supervised if a field needs to be left empty?
		DocInstance currentDoc = (DocInstance) context.getGeneric();
		Root root = currentDoc.getRoot();
		ZoneText zoneText = root.find(ZoneText.class);
		ObservableList<ZoneTextInstance> zoneTextInstances = (ObservableList) currentDoc.getHolders(zoneText).toObservableList().filtered(zt -> "reality".equals(((ZoneTextInstance) zt).getImgFilter().getValue()));
		BooleanBinding binding = Bindings.createBooleanBinding(() -> {
			if (zoneTextInstances.isEmpty()) {
				return false;
			} else {
				// If any field is empty, return false otherwise true
				return !zoneTextInstances.stream().anyMatch(g -> "".equals(g.getValue().toString()));
			}
		}, zoneTextInstances);
		return reverse ? binding.not() : binding;
	}
}
