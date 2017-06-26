package org.genericsystem.cv.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.Zone;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;

/**
 * Ongoing work (not yet implemented)
 * @author Pierrik Lassalas
 *
 */
@SystemGeneric
@PropertyConstraint
@Components({ ZoneGeneric.class, ImgFilter.class })
public class Score implements Generic {

	public static class ScoreInstance implements Generic {

		public ZoneInstance getZone() {
			return (ZoneInstance) this.getComponent(0);
		}

		public ImgFilterInstance getImgFilter() {
			return (ImgFilterInstance) this.getComponent(1);
		}

	}

	public ScoreInstance computeScoreInstance(ZoneInstance zoneInstance, ImgFilterInstance imgFilterInstance) {
		// zoneInstance.getHolders(engine.find(ZoneText.class));
		return null;
	}
}
