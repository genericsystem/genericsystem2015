package org.genericsystem.cv.nn;

import org.deeplearning4j.earlystopping.EarlyStoppingConfiguration;
import org.deeplearning4j.earlystopping.trainer.EarlyStoppingGraphTrainer;
import org.deeplearning4j.nn.graph.ComputationGraph;
import org.deeplearning4j.nn.transferlearning.TransferLearningHelper;
import org.nd4j.linalg.dataset.DataSet;
import org.nd4j.linalg.dataset.api.MultiDataSet;
import org.nd4j.linalg.dataset.api.iterator.DataSetIterator;
import org.nd4j.linalg.dataset.api.iterator.MultiDataSetIterator;

public class EarlyStoppingGraphFeaturizedTrainer extends EarlyStoppingGraphTrainer {
	private TransferLearningHelper transferLearningHelper;

	public EarlyStoppingGraphFeaturizedTrainer(EarlyStoppingConfiguration<ComputationGraph> esConfig, TransferLearningHelper transferLearningHelper,
			DataSetIterator train) {
		super(esConfig, transferLearningHelper.unfrozenGraph(), train, null);
		this.transferLearningHelper = transferLearningHelper;
	}

	public EarlyStoppingGraphFeaturizedTrainer(EarlyStoppingConfiguration<ComputationGraph> esConfig, TransferLearningHelper transferLearningHelper, MultiDataSetIterator train) {
		super(esConfig, transferLearningHelper.unfrozenGraph(), train, null);
		this.transferLearningHelper = transferLearningHelper;
	}

	@Override
	protected void fit(DataSet ds) {
		transferLearningHelper.fitFeaturized(ds);
	}

	@Override
	protected void fit(MultiDataSet mds) {
		transferLearningHelper.fitFeaturized((org.nd4j.linalg.dataset.MultiDataSet) mds);
	}
}
