package org.genericsystem.cv.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.cv.utils.Levenshtein;
import org.genericsystem.cv.model.LevDistance.LevDistanceInstance;

/**
 * This class stores the Levenshtein distance between two {@link ZoneText}.
 * 
 * @author Pierrik Lassalas
 *
 */
@SystemGeneric
@PropertyConstraint
@Components({ ZoneText.class, ZoneText.class })
@InstanceClass(LevDistanceInstance.class)
public class LevDistance implements Generic {

	public static class LevDistanceInstance implements Generic {

		public ZoneTextInstance getFirstZoneText() {
			return (ZoneTextInstance) this.getComponent(0);
		}

		public ZoneTextInstance getSecondZoneText() {
			return (ZoneTextInstance) this.getComponent(1);
		}
	}

	// TODO: remove method?
//	public LevDistanceInstance computeLevDistance(ZoneTextInstance zt1, ZoneTextInstance zt2) {
//		int d = Levenshtein.distance((String) zt1.getValue(), (String) zt2.getValue());
//		return (LevDistanceInstance) setInstance(d, zt1, zt2);
//	}

	public LevDistanceInstance setLevDistance(Integer distance, ZoneTextInstance zt1, ZoneTextInstance zt2) {
		return (LevDistanceInstance) setInstance(distance, zt1, zt2);
	}

	public LevDistanceInstance getLevDistance(ZoneTextInstance zt1, ZoneTextInstance zt2) {
		return (LevDistanceInstance) getInstance(zt1, zt2);
	}

}
