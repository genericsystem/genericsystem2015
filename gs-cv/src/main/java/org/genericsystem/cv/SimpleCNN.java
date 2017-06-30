package org.genericsystem.cv;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import org.apache.commons.io.FilenameUtils;
import org.datavec.api.io.filters.BalancedPathFilter;
import org.datavec.api.io.labels.ParentPathLabelGenerator;
import org.datavec.api.split.FileSplit;
import org.datavec.api.split.InputSplit;
import org.datavec.image.loader.BaseImageLoader;
import org.datavec.image.recordreader.ImageRecordReader;
import org.datavec.image.transform.FlipImageTransform;
import org.datavec.image.transform.ImageTransform;
import org.datavec.image.transform.WarpImageTransform;
import org.deeplearning4j.datasets.datavec.RecordReaderDataSetIterator;
import org.deeplearning4j.earlystopping.EarlyStoppingConfiguration;
import org.deeplearning4j.earlystopping.EarlyStoppingResult;
import org.deeplearning4j.earlystopping.saver.LocalFileModelSaver;
import org.deeplearning4j.earlystopping.scorecalc.DataSetLossCalculator;
import org.deeplearning4j.earlystopping.termination.MaxEpochsTerminationCondition;
import org.deeplearning4j.earlystopping.termination.ScoreImprovementEpochTerminationCondition;
import org.deeplearning4j.earlystopping.trainer.EarlyStoppingTrainer;
import org.deeplearning4j.eval.Evaluation;
import org.deeplearning4j.nn.api.OptimizationAlgorithm;
import org.deeplearning4j.nn.conf.GradientNormalization;
import org.deeplearning4j.nn.conf.MultiLayerConfiguration;
import org.deeplearning4j.nn.conf.NeuralNetConfiguration;
import org.deeplearning4j.nn.conf.Updater;
import org.deeplearning4j.nn.conf.inputs.InputType;
import org.deeplearning4j.nn.conf.layers.ConvolutionLayer;
import org.deeplearning4j.nn.conf.layers.DenseLayer;
import org.deeplearning4j.nn.conf.layers.OutputLayer;
import org.deeplearning4j.nn.conf.layers.SubsamplingLayer;
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork;
import org.deeplearning4j.nn.weights.WeightInit;
import org.deeplearning4j.optimize.listeners.ScoreIterationListener;
import org.deeplearning4j.util.ModelSerializer;
import org.nd4j.linalg.activations.Activation;
import org.nd4j.linalg.dataset.api.iterator.DataSetIterator;
import org.nd4j.linalg.dataset.api.preprocessor.DataNormalization;
import org.nd4j.linalg.dataset.api.preprocessor.ImagePreProcessingScaler;
import org.nd4j.linalg.lossfunctions.LossFunctions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SimpleCNN {
	protected static final Logger log = LoggerFactory.getLogger(SimpleCNN.class);

	private static final int seed = 123;
	private static Random rng = new Random(seed);
	private static final String[] allowedExtensions = BaseImageLoader.ALLOWED_FORMATS;
	public static final Random randNumGen = new Random(seed);

	private static int height = 250;
	private static int width = 200;
	private static int channels = 3;
	protected static int iterations = 1;

	public static void main(String[] args) throws Exception {
		double learningRate = 0.1;
		int batchSize = 1;
		int nEpochs = 100;

		File parentDir = new File(System.getProperty("user.dir"), "training");
		FileSplit filesInDir = new FileSplit(parentDir, allowedExtensions, randNumGen);
		ParentPathLabelGenerator labelMaker = new ParentPathLabelGenerator();
		BalancedPathFilter pathFilter = new BalancedPathFilter(randNumGen, allowedExtensions, labelMaker, 0, 0, 10, 0);

		InputSplit[] filesInDirSplit = filesInDir.sample(pathFilter, 70, 15, 15);
		InputSplit trainData = filesInDirSplit[0];
		InputSplit validData = filesInDirSplit[1];
		InputSplit testData = filesInDirSplit[2];

		ImageRecordReader recordReader = new ImageRecordReader(height, width, channels, labelMaker);

		recordReader.initialize(trainData, null);
		List<String> labels = recordReader.getLabels();
		int outputNum = recordReader.numLabels();

		MultiLayerConfiguration conf = new NeuralNetConfiguration.Builder()
				.seed(seed)
				.iterations(iterations)
				.regularization(false)
				.gradientNormalization(GradientNormalization.RenormalizeL2PerParamType)
				.activation(Activation.RELU)
				.learningRate(learningRate)
				.weightInit(WeightInit.RELU)
				.optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
				.updater(Updater.RMSPROP)
				.list()
				.layer(0, new ConvolutionLayer.Builder(new int[] { 5, 5 }, new int[] { 2, 2 }, new int[] { 2, 2 })
						.name("inputLayer")
						.nIn(channels)
						.nOut(96)
						.biasInit(0).build())
				.layer(1, maxPool("maxpool1"))
				.layer(2, new ConvolutionLayer.Builder(new int[] { 5, 5 }, new int[] { 1, 1 }, new int[] { 1, 1 })
						.name("convLayer")
						.nOut(256)
						.biasInit(0).build())
				.layer(3, maxPool("maxpool2"))
				.layer(4, new ConvolutionLayer.Builder(new int[] { 3, 3 }, new int[] { 1, 1 }, new int[] { 1, 1 })
						.name("convLayer2")
						.nOut(256)
						.biasInit(0).build())
				.layer(5, maxPool("maxpool3"))
				.layer(6, new DenseLayer.Builder().nOut(512).build())
				.layer(7, new DenseLayer.Builder().nOut(256).build())
				.layer(8, new OutputLayer.Builder(LossFunctions.LossFunction.MCXENT)
						.nOut(outputNum)
						.activation(Activation.SOFTMAX).build())
				.setInputType(InputType.convolutional(height, width, channels))
				.build();

		MultiLayerNetwork model = new MultiLayerNetwork(conf);
		model.init();
		model.setListeners(new ScoreIterationListener(10)); // Print score every 10 parameter updates

		String tempDir = System.getProperty("java.io.tmpdir");
		String saveDirectory = FilenameUtils.concat(tempDir, "EarlyStoppingIntermediaryResults/");
		Paths.get(saveDirectory).toFile().mkdirs();
		EarlyStoppingConfiguration<MultiLayerNetwork> esConf = new EarlyStoppingConfiguration.Builder<MultiLayerNetwork>()
				.epochTerminationConditions(new MaxEpochsTerminationCondition(nEpochs))
				.evaluateEveryNEpochs(1)
				.epochTerminationConditions(new ScoreImprovementEpochTerminationCondition(20))
				.scoreCalculator(new DataSetLossCalculator(getDataSetIterator(recordReader, validData, null, batchSize, outputNum), true))
				.modelSaver(new LocalFileModelSaver(saveDirectory))
				.build();

		DataSetIterator dataIter = getDataSetIterator(recordReader, trainData, null, batchSize, outputNum);
		EarlyStoppingTrainer trainer = new EarlyStoppingTrainer(esConf, conf, dataIter);

		EarlyStoppingResult<MultiLayerNetwork> result = trainer.fit();

		ImageTransform flipTransform1 = new FlipImageTransform(rng);
		// ImageTransform rotateTransform = new RotateImageTransform(rng, 20, 20, 100, 5);
		ImageTransform warpTransform = new WarpImageTransform(rng, 42);
		List<ImageTransform> transforms = Arrays.asList(new ImageTransform[] { flipTransform1, warpTransform/* , rotateTransform */ });
		for (ImageTransform transform : transforms) {
			System.out.print("\nTraining on transformation: " + transform.getClass().toString() + "\n\n");
			dataIter = getDataSetIterator(recordReader, trainData, transform, batchSize, outputNum);
			trainer = new EarlyStoppingTrainer(esConf, result.getBestModel(), dataIter);
			result = trainer.fit();
		}

		log.info("Evaluate model....");
		dataIter = getDataSetIterator(recordReader, testData, null, batchSize, outputNum);
		Evaluation eval = model.evaluate(dataIter, labels);
		log.info(eval.stats(true));

		File modelFile = new File("TrainedModel-" + System.currentTimeMillis() + ".zip");
		ModelSerializer.writeModel(model, modelFile, true);
		log.info("Model saved to " + modelFile);
	}

	private static DataSetIterator getDataSetIterator(ImageRecordReader recordReader, InputSplit data, ImageTransform transform, int batchSize, int outputNum) {
		try {
			recordReader.initialize(data, transform);
		} catch (IOException e) {
			log.warn("Impossible to initialize recordReader", e);
		}
		DataSetIterator dataIter = new RecordReaderDataSetIterator(recordReader, batchSize, 1, outputNum);
		DataNormalization scaler = new ImagePreProcessingScaler(-1, 1);
		scaler.fit(dataIter);
		dataIter.setPreProcessor(scaler);
		return dataIter;
	}

	private static SubsamplingLayer maxPool(String name) {
		return new SubsamplingLayer.Builder(new int[] { 2, 2 }, new int[] { 2, 2 }).name(name).build();
	}
}
