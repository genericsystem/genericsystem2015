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

@SystemGeneric
@Dependencies({ ZoneNum.class, ZoneX.class, ZoneY.class, ZoneW.class, ZoneH.class })
@Components(DocClass.class)
@InstanceClass(ZoneInstance.class)
public class ZoneGeneric implements Generic {

	public static class ZoneInstance implements Generic {

		public DocClassInstance getDocClass() {
			return (DocClassInstance) this.getHolder(getRoot().find(DocClass.class));
		}

	}

	public ZoneInstance addZone(int numZone, DocClassInstance docClass) {
		return (ZoneInstance) setInstance(numZone, docClass);
	}

	public ZoneInstance getZone(int numZone, DocClassInstance docClass) {
		return (ZoneInstance) getInstance(numZone, docClass);
	}

	@SystemGeneric
	@Components(ZoneGeneric.class)
	@InstanceValueClassConstraint(Integer.class)
	public static class ZoneNum {
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