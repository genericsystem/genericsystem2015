package org.genericsystem.reinforcer;

import static org.apache.spark.sql.functions.input_file_name;
import static org.apache.spark.sql.functions.regexp_extract;

import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.spark.SparkConf;
import org.apache.spark.ml.Pipeline;
import org.apache.spark.ml.PipelineStage;
import org.apache.spark.ml.classification.LinearSVC;
import org.apache.spark.ml.classification.OneVsRest;
import org.apache.spark.ml.evaluation.MulticlassClassificationEvaluator;
import org.apache.spark.ml.feature.HashingTF;
import org.apache.spark.ml.feature.IDF;
import org.apache.spark.ml.feature.IndexToString;
import org.apache.spark.ml.feature.StringIndexer;
import org.apache.spark.ml.feature.Tokenizer;
import org.apache.spark.ml.param.ParamMap;
import org.apache.spark.ml.tuning.CrossValidator;
import org.apache.spark.ml.tuning.CrossValidatorModel;
import org.apache.spark.ml.tuning.ParamGridBuilder;
import org.apache.spark.sql.Dataset;
import org.apache.spark.sql.Row;
import org.apache.spark.sql.SparkSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SVMClassifier {

	private static final Logger logger = LoggerFactory.getLogger(SVMClassifier.class);

	public static void main(String[] args) {
		SparkConf sparkConf = new SparkConf().setMaster("local[*]");
		final SparkSession spark = SparkSession
				.builder()
				.config(sparkConf)
				.appName("SVMClassifier")
				.getOrCreate();
		new SVMClassifier().trainModel(spark);
		spark.stop();
	}

	public Dataset<Row> loadData(SparkSession spark, String path) {
		Dataset<Row> data = spark.read().text(path).withColumn("label", input_file_name());
		return data.withColumn("label", regexp_extract(data.col("label"), ".*/([^/]*)/[^/]*", 1)).cache();
	}

	public void trainModel(SparkSession spark) {
		Dataset<Row> data = loadData(spark, "pieces/text/*");
		Dataset<Row>[] splits = data.randomSplit(new double[] {0.7, 0.3});
		Dataset<Row> trainData = splits[0];
		Dataset<Row> validData = splits[1];

		StringIndexer indexer = new StringIndexer()
				.setInputCol("label")
				.setOutputCol("labelIndex")
				.setHandleInvalid("keep");
		Tokenizer tokenizer = new Tokenizer()
				.setInputCol("value")
				.setOutputCol("words");
		HashingTF hashingTF = new HashingTF()
				.setInputCol(tokenizer.getOutputCol())
				.setOutputCol("rawFeatures")
				.setNumFeatures(20);
		IDF idf = new IDF()
				.setInputCol(hashingTF.getOutputCol())
				.setOutputCol("features");
		LinearSVC lsvc = new LinearSVC()
				.setMaxIter(100)
				.setRegParam(0.1);
		OneVsRest ovr = new OneVsRest()
				.setClassifier(lsvc)
				.setFeaturesCol(idf.getOutputCol())
				.setLabelCol(indexer.getOutputCol());
		IndexToString converter = new IndexToString()
				.setInputCol("prediction")
				.setOutputCol("origPrediction");

		Pipeline pipeline = new Pipeline()
				.setStages(new PipelineStage[] { indexer, tokenizer, hashingTF, idf, ovr, converter });

		ParamMap[] paramGrid = new ParamGridBuilder()
				.addGrid(hashingTF.numFeatures(), new int[] { 20 , 50, 200, 1000 })
				.addGrid(lsvc.maxIter(), new int[] { 20, 50, 100, 200 })
				.addGrid(lsvc.regParam(), new double[] { 0.05, 0.1 })
				.build();

		MulticlassClassificationEvaluator evaluator = new MulticlassClassificationEvaluator()
				.setMetricName("accuracy")
				.setLabelCol(indexer.getOutputCol());

		CrossValidator cv = new CrossValidator()
				.setEstimator(pipeline)
				.setEvaluator(evaluator)
				.setEstimatorParamMaps(paramGrid)
				.setNumFolds(3);

		CrossValidatorModel cvModel = cv.fit(trainData);

		Dataset<Row> predictions = cvModel.transform(validData);
		double accuracy = evaluator.evaluate(predictions);
		logger.info("Test error = {}.", 1 - accuracy);

		DateFormat format = new SimpleDateFormat("yyyyMMddHHmmss");
		try {
			cvModel.save("SVMModel-" + format.format(new Date()));
		} catch (IOException e) {
			throw new RuntimeException("Exception while saving trained model", e);
		}
	}	
}
