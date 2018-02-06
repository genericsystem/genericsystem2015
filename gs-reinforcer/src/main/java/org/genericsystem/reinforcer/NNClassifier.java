package org.genericsystem.reinforcer;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Random;

import org.datavec.api.conf.Configuration;
import org.datavec.api.io.filters.BalancedPathFilter;
import org.datavec.api.io.labels.ParentPathLabelGenerator;
import org.datavec.api.records.reader.impl.FileRecordReader;
import org.datavec.api.split.FileSplit;
import org.datavec.api.split.InputSplit;
import org.deeplearning4j.datasets.datavec.RecordReaderDataSetIterator;
import org.deeplearning4j.earlystopping.EarlyStoppingConfiguration;
import org.deeplearning4j.earlystopping.EarlyStoppingResult;
import org.deeplearning4j.earlystopping.listener.EarlyStoppingListener;
import org.deeplearning4j.earlystopping.scorecalc.DataSetLossCalculator;
import org.deeplearning4j.earlystopping.termination.MaxEpochsTerminationCondition;
import org.deeplearning4j.earlystopping.termination.ScoreImprovementEpochTerminationCondition;
import org.deeplearning4j.earlystopping.trainer.EarlyStoppingTrainer;
import org.deeplearning4j.eval.Evaluation;
import org.deeplearning4j.models.embeddings.loader.WordVectorSerializer;
import org.deeplearning4j.models.embeddings.wordvectors.WordVectors;
import org.deeplearning4j.nn.conf.GradientNormalization;
import org.deeplearning4j.nn.conf.MultiLayerConfiguration;
import org.deeplearning4j.nn.conf.NeuralNetConfiguration;
import org.deeplearning4j.nn.conf.layers.DenseLayer;
import org.deeplearning4j.nn.conf.layers.OutputLayer;
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork;
import org.deeplearning4j.nn.weights.WeightInit;
import org.deeplearning4j.optimize.listeners.ScoreIterationListener;
import org.deeplearning4j.text.tokenization.tokenizer.preprocessor.CommonPreprocessor;
import org.deeplearning4j.text.tokenization.tokenizerfactory.DefaultTokenizerFactory;
import org.deeplearning4j.text.tokenization.tokenizerfactory.TokenizerFactory;
import org.nd4j.linalg.activations.Activation;
import org.nd4j.linalg.dataset.api.iterator.DataSetIterator;
import org.nd4j.linalg.dataset.api.preprocessor.DataNormalization;
import org.nd4j.linalg.dataset.api.preprocessor.NormalizerStandardize;
import org.nd4j.linalg.learning.config.Nesterovs;
import org.nd4j.linalg.lossfunctions.LossFunctions.LossFunction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class NNClassifier {
	protected static final Logger log = LoggerFactory.getLogger(NNClassifier.class);

	private static final int seed = 123;
	private static final String[] allowedExtensions = new String[] { "txt" };
	public static final Random randNumGen = new Random(seed);
	private static final File frModel = new File("frWiki_no_phrase_no_postag_500_cbow_cut10.bin");


	public static void main(String[] args) throws Exception {
		double learningRate = 0.001;
		double momentum = 0.9;
		int batchSize = 1;
		int nEpochs = 100;
		int iterations = 1;

		File parentDir = new File(System.getProperty("user.dir"), "pieces/text");
		FileSplit filesInDir = new FileSplit(parentDir, allowedExtensions, randNumGen);
		ParentPathLabelGenerator labelMaker = new ParentPathLabelGenerator();
		BalancedPathFilter pathFilter = new BalancedPathFilter(randNumGen, allowedExtensions, labelMaker, 0, 0, 20, 0);

		InputSplit[] filesInDirSplit = filesInDir.sample(pathFilter, 70, 15, 15);
		InputSplit trainData = filesInDirSplit[0];
		InputSplit validData = filesInDirSplit[1];
		InputSplit testData = filesInDirSplit[2];

		WordVectors dictionary = WordVectorSerializer.readWord2VecModel(frModel, true);
		TokenizerFactory tokenizer = new DefaultTokenizerFactory();
		tokenizer.setTokenPreProcessor(new CommonPreprocessor());

		FileRecordReader recordReader = new VecRecordReader(dictionary, tokenizer);
		Configuration readerConf = new Configuration();
		readerConf.setBoolean(FileRecordReader.APPEND_LABEL, true);
		recordReader.initialize(readerConf, trainData);
		List<String> labels = recordReader.getLabels();
		int outputNum = labels.size();

		MultiLayerConfiguration conf = new NeuralNetConfiguration.Builder()
				.seed(seed)
				.weightInit(WeightInit.XAVIER)
				.iterations(iterations)
				.activation(Activation.TANH)
				.learningRate(learningRate)
				.updater(new Nesterovs(momentum))
				.gradientNormalization(GradientNormalization.RenormalizeL2PerLayer)
				.regularization(true)
				.l2(1e-4)
				.list()
				.layer(0, new DenseLayer.Builder().nIn(500).nOut(1024).build())
				.layer(1, new DenseLayer.Builder().nIn(1024).nOut(1024).build())
				.layer(2, new DenseLayer.Builder().nIn(1024).nOut(1024).build())
				.layer(3, new OutputLayer.Builder(LossFunction.NEGATIVELOGLIKELIHOOD).nIn(1024).nOut(outputNum).activation(Activation.SOFTMAX).build())
				.pretrain(false)
				.backprop(true)
				.build();

		MultiLayerNetwork model = new MultiLayerNetwork(conf);
		model.init();
		model.setListeners(new ScoreIterationListener(10));

		DataNormalization normalizer = new NormalizerStandardize();
		DataSetIterator dataIter = getDataSetIterator(recordReader, readerConf, null, trainData, batchSize, outputNum);
		normalizer.fit(dataIter);
		dataIter.setPreProcessor(normalizer);

		DataSetIterator validIter = getDataSetIterator(recordReader, readerConf, normalizer, validData, batchSize, outputNum);
		EarlyStoppingConfiguration<MultiLayerNetwork> esConf = new EarlyStoppingConfiguration.Builder<MultiLayerNetwork>()
				.epochTerminationConditions(new MaxEpochsTerminationCondition(nEpochs))
				.evaluateEveryNEpochs(1)
				.epochTerminationConditions(new ScoreImprovementEpochTerminationCondition(20))
				.scoreCalculator(new DataSetLossCalculator(validIter, false))
				.build();

		EarlyStoppingTrainer trainer = new EarlyStoppingTrainer(esConf, model, dataIter);
		trainer.setListener(new EarlyStoppingListener<MultiLayerNetwork>() {

			@Override
			public void onStart(EarlyStoppingConfiguration<MultiLayerNetwork> esConfig, MultiLayerNetwork net) {
			}

			@Override
			public void onEpoch(int epochNum, double score, EarlyStoppingConfiguration<MultiLayerNetwork> esConfig, MultiLayerNetwork net) {
				log.info("Epoch {}, score {}.", epochNum, score);
			}

			@Override
			public void onCompletion(EarlyStoppingResult<MultiLayerNetwork> esResult) {
			}
		});

		// Early stopping training not working
		//		log.info("Early stopping training");
		//		EarlyStoppingResult<MultiLayerNetwork> result = trainer.fit();
		//		log.info("Evaluate model....");
		//		dataIter = getDataSetIterator(recordReader, readerConf, normalizer, testData, batchSize, outputNum);
		//		Evaluation eval = result.getBestModel().evaluate(dataIter);
		//		log.info(eval.stats(true));

		log.info("Training without early stopping");
		for (int i = 0; i < nEpochs; i++) {
			model.fit(dataIter);
			log.info("Completed epoch {}", i);
			dataIter.reset();
		}
		log.info("Evaluate model....");
		dataIter = getDataSetIterator(recordReader, readerConf, normalizer, testData, batchSize, outputNum);
		Evaluation eval = model.evaluate(dataIter);
		log.info(eval.stats(true));

		//		File modelFile = new File("TrainedModel-" + System.currentTimeMillis() + ".zip");
		//		ModelSerializer.writeModel(model, modelFile, true);
		//		log.info("Model saved to " + modelFile);
	}

	private static DataSetIterator getDataSetIterator(FileRecordReader recordReader, Configuration conf, DataNormalization normalizer, InputSplit data, int batchSize, int outputNum) {
		try {
			recordReader.initialize(conf, data);
		} catch (InterruptedException | IOException e) {
			log.warn("Impossible to initialize recordReader", e);
		}
		DataSetIterator dataIter = new RecordReaderDataSetIterator(recordReader, batchSize, 500, outputNum);
		if (normalizer != null) {
			dataIter.setPreProcessor(normalizer);
		}
		return dataIter;
	}
}