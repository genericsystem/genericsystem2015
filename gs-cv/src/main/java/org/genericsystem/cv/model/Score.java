package org.genericsystem.cv.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.MeanLevenshtein.MeanLevenshteinInstance;
import org.genericsystem.cv.model.Score.ScoreInstance;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;

/**
 * This class stores the score of a given couple of {@link ZoneGeneric} and {@link ImgFilter} When an image is processed, the score represents the probability for a given filter on a given zone to get an accurate text value after OCR.
 * 
 * @author Jean Mathorel
 * @author Pierrik Lassalas
 */
@SystemGeneric
@PropertyConstraint
@Components({ ZoneGeneric.class, ImgFilter.class })
@InstanceClass(ScoreInstance.class)
public class Score implements Generic {

	public static class ScoreInstance implements Generic {

		public ZoneInstance getZone() {
			return (ZoneInstance) getComponent(0);
		}

		public ImgFilterInstance getImgFilter() {
			return (ImgFilterInstance) getComponent(1);
		}

		public MeanLevenshteinInstance setMeanLev(Float meanValue) {
			return (MeanLevenshteinInstance) setHolder(getRoot().find(MeanLevenshtein.class), meanValue);
		}

		public MeanLevenshteinInstance getMeanLev(Float meanValue) {
			return (MeanLevenshteinInstance) getHolder(getRoot().find(MeanLevenshtein.class), meanValue);
		}
	}

	public ScoreInstance setScore(Float score, ZoneInstance zoneInstance, ImgFilterInstance imgFilterInstance) {
		return (ScoreInstance) setInstance(score, zoneInstance, imgFilterInstance);
	}

	public ScoreInstance getScore(ZoneInstance zoneInstance, ImgFilterInstance imgFilterInstance) {
		return (ScoreInstance) getInstance(zoneInstance, imgFilterInstance);
	}
}
