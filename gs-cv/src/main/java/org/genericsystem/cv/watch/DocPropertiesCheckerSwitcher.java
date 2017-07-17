package org.genericsystem.cv.watch;

import java.io.File;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Root;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.TagSwitcher;

import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;

public class DocPropertiesCheckerSwitcher {

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
		File file = new File(System.getProperty("user.dir") + "/../gs-cv/classes/"
				+ docClassInstance.getValue().toString() + "/zones/zones.json");
		if (reverse)
			return new SimpleBooleanProperty(file.exists()).not();
		else
			return new SimpleBooleanProperty(file.exists());
	}

	@SuppressWarnings("rawtypes")
	public static ObservableValue<Boolean> isDocOcrd(Context context, boolean reverse) {
		// TODO: verify / test
		DocInstance currentDoc = (DocInstance) context.getGeneric();
		Root root = currentDoc.getRoot();
		Snapshot<ZoneTextInstance> zoneTextInstances = (Snapshot) currentDoc.getHolders(root.find(ZoneText.class));
		if (reverse)
			return new SimpleBooleanProperty(zoneTextInstances != null).not();
		else
			return new SimpleBooleanProperty(zoneTextInstances != null);
	}

	@SuppressWarnings("unchecked")
	public static ObservableValue<Boolean> isDocSupervised(Context context, boolean reverse) {
		// TODO: will a document be considered as not supervised if a
		// field needs to be left empty?
		
		DocInstance currentDoc = (DocInstance) context.getGeneric();
		Root root = currentDoc.getRoot();
		Snapshot<ZoneTextInstance> zoneTextInstances = (Snapshot) currentDoc.getHolders(root.find(ZoneText.class))
				.filter(zt -> "reality".equals(((ZoneTextInstance) zt).getImgFilter().getValue()));
		boolean supervised;
		if (zoneTextInstances == null) {
			supervised = false;
		} else {
			// If any field is empty, return false otherwise true
			supervised = !zoneTextInstances.stream().anyMatch(g -> "".equals(g.getValue().toString()));
		}
		if (reverse)
			return new SimpleBooleanProperty(supervised).not();
		else
			return new SimpleBooleanProperty(supervised);
	}

}
