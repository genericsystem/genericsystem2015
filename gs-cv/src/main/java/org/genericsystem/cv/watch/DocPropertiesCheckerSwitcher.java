package org.genericsystem.cv.watch;

import java.io.File;

import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.TagSwitcher;

import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;

public class DocPropertiesCheckerSwitcher {

	public static class DOC_DEZONED implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isClassZoneFilePresent(context);
		}
	}
	
	public static class DOC_NOT_DEZONED implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isClassZoneFilePresent(context).not();
		}
	}
	
	public static class DOC_OCRD implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isDocOcrd(context);
		}
	}
	
	public static class DOC_NOT_OCRD implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isDocOcrd(context).not();
		}
	}
	
	public static class DOC_SUPERVISED implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isDocSupervised(context);
		}
	}
	
	public static class DOC_NOT_SUPERVISED implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isDocSupervised(context).not();
		}
	}
	
	public static SimpleBooleanProperty isClassZoneFilePresent (Context context) {
		DocInstance currentDoc = (DocInstance) context.getGeneric();
		DocClassInstance docClassInstance = currentDoc.getDocClass();
		File file = new File(System.getProperty("user.dir") + "/../gs-cv/classes/" + docClassInstance.getValue().toString() + "/zones/zones.json");
		return new SimpleBooleanProperty(file.exists());
	}
	
	public static SimpleBooleanProperty isDocOcrd (Context context) {
		// TODO: implement
		return new SimpleBooleanProperty(true);
	}
	
	public static SimpleBooleanProperty isDocSupervised (Context context) {
		// TODO: implement
		return new SimpleBooleanProperty(true);
	}
	
}
