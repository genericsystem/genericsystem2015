package org.genericsystem.cv.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.Zone;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ZoneGeneric.ZoneH;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneGeneric.ZoneW;
import org.genericsystem.cv.model.ZoneGeneric.ZoneX;
import org.genericsystem.cv.model.ZoneGeneric.ZoneY;
import org.opencv.core.Rect;

/**
 * This class stores zones for given class of documents.
 * 
 * @author Jean Mathorel
 * @author Pierrik Lassalas
 *
 */
@SystemGeneric
@Dependencies({ ZoneX.class, ZoneY.class, ZoneW.class, ZoneH.class })
@Components(DocClass.class)
@InstanceClass(ZoneInstance.class)
public class ZoneGeneric implements Generic {

	public static class ZoneInstance implements Generic {

		public DocClassInstance getDocClass() {
			return (DocClassInstance) this.getHolder(getRoot().find(DocClass.class));
		}
		
		public Zone getZoneObject(){
			int num = (int) getHolder(getRoot().find(ZoneGeneric.class)).getValue();
			int x = (int) getHolder(getRoot().find(ZoneX.class)).getValue();
			int y = (int) getHolder(getRoot().find(ZoneY.class)).getValue();
			int width = (int) getHolder(getRoot().find(ZoneW.class)).getValue();
			int height = (int) getHolder(getRoot().find(ZoneH.class)).getValue();
			return new Zone(num, new Rect(x, y, width, height));
		}
		
	}

	public ZoneInstance setZone(int numZone, DocClassInstance docClass) {
		return (ZoneInstance) setInstance(numZone, docClass);
	}

	public ZoneInstance getZone(int numZone, DocClassInstance docClass) {
		return (ZoneInstance) getInstance(numZone, docClass);
	}

	@SystemGeneric
	@Components(ZoneGeneric.class)
	@InstanceValueClassConstraint(Integer.class)
	public static class ZoneX {
	}

	@SystemGeneric
	@Components(ZoneGeneric.class)
	@InstanceValueClassConstraint(Integer.class)
	public static class ZoneY {
	}

	@SystemGeneric
	@Components(ZoneGeneric.class)
	@InstanceValueClassConstraint(Integer.class)
	public static class ZoneW {
	}

	@SystemGeneric
	@Components(ZoneGeneric.class)
	@InstanceValueClassConstraint(Integer.class)
	public static class ZoneH {
	}

}