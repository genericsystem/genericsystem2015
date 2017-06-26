package org.genericsystem.cv.model;

import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ZoneGeneric.ZoneH;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneGeneric.ZoneW;
import org.genericsystem.cv.model.ZoneGeneric.ZoneX;
import org.genericsystem.cv.model.ZoneGeneric.ZoneY;

@SystemGeneric
@InstanceClass(DocClassInstance.class)
public class DocClass implements Generic {

	public static class DocClassInstance implements Generic {
		public ZoneInstance addZone(int num, int x, int y, int width, int height) {
			ZoneInstance zoneInstance = (ZoneInstance) setHolder(getRoot().find(ZoneGeneric.class), num);
			zoneInstance.setHolder(getRoot().find(ZoneX.class), x);
			zoneInstance.setHolder(getRoot().find(ZoneY.class), y);
			zoneInstance.setHolder(getRoot().find(ZoneW.class), width);
			zoneInstance.setHolder(getRoot().find(ZoneH.class), height);
			return zoneInstance;
		}

		public DocInstance addDoc(DocClassInstance docClassInstance, Generic docInstance, String filename){
			DocInstance doc = (DocInstance) docClassInstance.setHolder(docInstance, filename);
			return doc;
		}

		public ZoneInstance getZone(int num) {
			return (ZoneInstance) getHolder(getRoot().find(ZoneGeneric.class), num);

		}

	}

	public DocClassInstance addDocClass(String name) {
		return (DocClassInstance) setInstance(name);
	}

	public DocClassInstance getDocClass(String name) {
		return (DocClassInstance) getInstance(name);
	}

}
