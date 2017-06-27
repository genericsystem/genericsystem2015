package org.genericsystem.cv.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.model.Score.ScoreInstance;
import org.genericsystem.cv.model.MeanLevenshtein.MeanLevenshteinInstance;

/**
 * 
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
			return (ScoreInstance) this.getBaseComponent();
		}

	}

	public MeanLevenshteinInstance addMeanLev(Float meanValue, ScoreInstance scoreInstance) {
		return (MeanLevenshteinInstance) setInstance(meanValue, scoreInstance);
	}

	public MeanLevenshteinInstance getMeanLev(ScoreInstance scoreInstance) {
		return (MeanLevenshteinInstance) getInstance(scoreInstance);
	}
}
