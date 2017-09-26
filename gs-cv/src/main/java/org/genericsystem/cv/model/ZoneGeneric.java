package org.genericsystem.cv.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ZoneGeneric.ZoneH;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneGeneric.ZoneNum;
import org.genericsystem.cv.model.ZoneGeneric.ZoneW;
import org.genericsystem.cv.model.ZoneGeneric.ZoneX;
import org.genericsystem.cv.model.ZoneGeneric.ZoneY;
import org.opencv.core.Rect;

/**
 * This class stores zones for given class of documents.
 * 
 * @author Jean Mathorel
 * @author Pierrik Lassalas
 */
@SystemGeneric
@Dependencies({ ZoneX.class, ZoneY.class, ZoneW.class, ZoneH.class, ZoneNum.class })
@Components(DocClass.class)
@InstanceClass(ZoneInstance.class)
public class ZoneGeneric implements Generic {

	public static class ZoneInstance implements Generic {

		public DocClassInstance getDocClass() {
			return (DocClassInstance) getHolder(getRoot().find(DocClass.class));
		}

		public Generic setZoneNum(int num) {
			return setHolder(getRoot().find(ZoneNum.class), num);
		}

		public Generic getZoneNum() {
			return getHolder(getRoot().find(ZoneNum.class));
		}

		public Rect getRect() {
			int x = (int) getHolder(getRoot().find(ZoneX.class)).getValue();
			int y = (int) getHolder(getRoot().find(ZoneY.class)).getValue();
			int width = (int) getHolder(getRoot().find(ZoneW.class)).getValue();
			int height = (int) getHolder(getRoot().find(ZoneH.class)).getValue();
			return new Rect(x, y, width, height);
		}

	}

	public ZoneInstance setZone(String zoneUid, DocClassInstance docClass) {
		return (ZoneInstance) setInstance(zoneUid, docClass);
	}

	public ZoneInstance getZone(String zoneUid, DocClassInstance docClass) {
		return (ZoneInstance) getInstance(zoneUid, docClass);
	}

	@SystemGeneric
	@Components(ZoneGeneric.class)
	@InstanceValueClassConstraint(Integer.class)
	public static class ZoneX implements Generic {
	}

	@SystemGeneric
	@Components(ZoneGeneric.class)
	@InstanceValueClassConstraint(Integer.class)
	public static class ZoneY implements Generic {
	}

	@SystemGeneric
	@Components(ZoneGeneric.class)
	@InstanceValueClassConstraint(Integer.class)
	public static class ZoneW implements Generic {
	}

	@SystemGeneric
	@Components(ZoneGeneric.class)
	@InstanceValueClassConstraint(Integer.class)
	public static class ZoneH implements Generic {
	}

	@SystemGeneric
	@Components(ZoneGeneric.class)
	@InstanceValueClassConstraint(Integer.class)
	public static class ZoneNum implements Generic {
	}

}
