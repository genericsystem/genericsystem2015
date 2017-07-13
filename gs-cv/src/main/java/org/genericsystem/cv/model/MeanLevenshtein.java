package org.genericsystem.cv.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.model.Score.ScoreInstance;
import org.genericsystem.cv.model.MeanLevenshtein.MeanLevenshteinInstance;

/**
 * This class stores the mean levenshtein distance for a given couple of
 * {@link ZoneGeneric} and {@link ImgFilter}, among all the documents in a
 * specific class.
 * 
 * @author Pierrik Lassalas
 *
 */
@SystemGeneric
@PropertyConstraint
@Components({ Score.class })
@InstanceClass(MeanLevenshteinInstance.class)
public class MeanLevenshtein implements Generic {

	public static class MeanLevenshteinInstance implements Generic {

		public ScoreInstance getScore() {
			return (ScoreInstance) getBaseComponent();
		}
	}

	public MeanLevenshteinInstance setMeanLev(Float meanValue, ScoreInstance scoreInstance) {
		return (MeanLevenshteinInstance) setInstance(meanValue, scoreInstance);
	}

	public MeanLevenshteinInstance getMeanLev(ScoreInstance scoreInstance) {
		return (MeanLevenshteinInstance) getInstance(scoreInstance);
	}
}
