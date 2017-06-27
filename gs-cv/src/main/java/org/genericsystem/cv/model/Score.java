package org.genericsystem.cv.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.Score.ScoreInstance;

/**
 * The score class is a relation between a text field (zone) and an image
 * filter.
 * 
 * When an image is treated with a given filter, and then OCR'd, the probability
 * of the text being correct is given by the score.
 * 
 * @author Pierrik Lassalas
 *
 */
@SystemGeneric
@PropertyConstraint
@Components({ ZoneGeneric.class, ImgFilter.class })
@InstanceClass(ScoreInstance.class)
public class Score implements Generic {

	public static class ScoreInstance implements Generic {

		public ZoneInstance getZone() {
			return (ZoneInstance) this.getComponent(0);
		}

		public ImgFilterInstance getImgFilter() {
			return (ImgFilterInstance) this.getComponent(1);
		}
	}

	public ScoreInstance addScore(Float score, ZoneInstance zoneInstance, ImgFilterInstance imgFilterInstance) {
		return (ScoreInstance) setInstance(score, zoneInstance, imgFilterInstance);
	}

	public ScoreInstance getScore(ZoneInstance zoneInstance, ImgFilterInstance imgFilterInstance) {
		return (ScoreInstance) getInstance(zoneInstance, imgFilterInstance);
	}
}
