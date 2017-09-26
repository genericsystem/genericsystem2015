package org.genericsystem.cv.model;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.Zone;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ZoneGeneric.ZoneH;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneGeneric.ZoneNum;
import org.genericsystem.cv.model.ZoneGeneric.ZoneW;
import org.genericsystem.cv.model.ZoneGeneric.ZoneX;
import org.genericsystem.cv.model.ZoneGeneric.ZoneY;
import org.opencv.core.Rect;

/**
 * This class stores the document class.
 * 
 * @author Jean Mathorel
 * @author Pierrik Lassalas
 */
@SystemGeneric
@InstanceClass(DocClassInstance.class)
public class DocClass implements Generic {

	public static class DocClassInstance implements Generic {

		public ZoneInstance setZone(Zone zone) {
			ZoneInstance zoneInstance = (ZoneInstance) setHolder(getRoot().find(ZoneGeneric.class), zone.getUid());
			Rect rect = zone.getRect();
			zoneInstance.setHolder(getRoot().find(ZoneX.class), rect.x);
			zoneInstance.setHolder(getRoot().find(ZoneY.class), rect.y);
			zoneInstance.setHolder(getRoot().find(ZoneW.class), rect.width);
			zoneInstance.setHolder(getRoot().find(ZoneH.class), rect.height);
			zoneInstance.setHolder(getRoot().find(ZoneNum.class), zone.getNum());
			return zoneInstance;
		}

		@SuppressWarnings({ "rawtypes", "unchecked" })
		public Snapshot<ZoneInstance> getZones() {
			return (Snapshot) getHolders(getRoot().find(ZoneGeneric.class));
		}

		@SuppressWarnings({ "rawtypes", "unchecked" })
		public Snapshot<DocInstance> getDocs() {
			return (Snapshot) getHolders(getRoot().find(Doc.class));
		}

		public DocInstance setDoc(Generic docInstance, String filename) {
			return (DocInstance) setHolder(docInstance, filename);
		}

		public DocInstance getDoc(Generic docInstance, String filename) {
			return (DocInstance) getHolder(docInstance, filename);
		}

		public ZoneInstance getZone(String uid) {
			return (ZoneInstance) getHolder(getRoot().find(ZoneGeneric.class), uid);
		}

		public ZoneInstance getZoneByNum(int num) {
			return (ZoneInstance) getHolders(getRoot().find(ZoneGeneric.class)).filter(zone -> num == ((Zone) zone).getNum()).first();
		}

	}

	public DocClassInstance setDocClass(String name) {
		return (DocClassInstance) setInstance(name);
	}

	public DocClassInstance getDocClass(String name) {
		return (DocClassInstance) getInstance(name);
	}

}
