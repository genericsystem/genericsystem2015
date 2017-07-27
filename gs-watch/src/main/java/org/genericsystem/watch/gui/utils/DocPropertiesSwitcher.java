package org.genericsystem.watch.gui.utils;

import java.io.File;

import org.genericsystem.common.Root;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
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

	public static ObservableValue<Boolean> isClassZoneFilePresent(Context context, boolean reverse) {
		DocInstance currentDoc = (DocInstance) context.getGeneric();
		DocClassInstance docClassInstance = currentDoc.getDocClass();
		ObjectProperty<File> file = new SimpleObjectProperty<>(new File(System.getProperty("user.dir") + "/../gs-cv/classes/" + docClassInstance.getValue().toString() + "/zones/zones.json"));
		BooleanBinding binding = Bindings.createBooleanBinding(() -> {
			return null != file.get() && file.get().exists();
		}, file);
		return reverse ? binding.not() : binding;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public static ObservableValue<Boolean> isDocOcrd(Context context, boolean reverse) {
		// TODO: modify the algorithm so each zone needs to be processed before returning true
		DocInstance currentDoc = (DocInstance) context.getGeneric();
		Root root = currentDoc.getRoot();
		ZoneText zoneText = root.find(ZoneText.class);
		ZoneGeneric zoneGeneric = root.find(ZoneGeneric.class);
		// ObservableList<ZoneGeneric> zoneGenerics = (ObservableList) zoneGeneric.getInstances().toObservableList(); // XXX maybe just a list
		ObservableList<ZoneTextInstance> zoneTextInstances = (ObservableList) currentDoc.getHolders(zoneText).toObservableList();
		BooleanBinding binding = Bindings.createBooleanBinding(() -> {
			// zoneGenerics.stream().allMatch(g -> {
			// zoneTextInstances.filtered(zti -> zti.getZoneNum());
			// });
			return !zoneTextInstances.isEmpty();
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
