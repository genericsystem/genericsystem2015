package org.genericsystem.cv.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.Levenshtein;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;

@SystemGeneric
@PropertyConstraint
@Components({ ZoneText.class, ZoneText.class })
public class LevDistance implements Generic {

	public static class LevDistanceInstance implements Generic {

		public ZoneTextInstance getFirstZoneText() {
			return (ZoneTextInstance) this.getComponent(0);
		}

		public ZoneTextInstance getSecondZoneText() {
			return (ZoneTextInstance) this.getComponent(1);
		}

	}

	public LevDistanceInstance computeLevDistance(ZoneTextInstance zt1, ZoneTextInstance zt2) {
		int d = Levenshtein.distance((String) zt1.getValue(), (String) zt2.getValue());
		return (LevDistanceInstance) setInstance(d, zt1, zt2);
	}

}
