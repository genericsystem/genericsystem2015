package org.genericsystem.reinforcer;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.io.FileUtils;
import org.datavec.api.records.Record;
import org.datavec.api.records.metadata.RecordMetaDataURI;
import org.datavec.api.records.reader.impl.FileRecordReader;
import org.datavec.api.writable.DoubleWritable;
import org.datavec.api.writable.IntWritable;
import org.datavec.api.writable.Writable;
import org.deeplearning4j.models.embeddings.wordvectors.WordVectors;
import org.deeplearning4j.text.tokenization.tokenizerfactory.TokenizerFactory;
import org.nd4j.linalg.api.ndarray.INDArray;


public class VecRecordReader extends FileRecordReader {

	private final WordVectors dictionary;
	private final TokenizerFactory tokenizer;

	public VecRecordReader(WordVectors dictionary, TokenizerFactory tokenizer) {
		this.dictionary = dictionary;
		this.tokenizer = tokenizer;
	}

	@Override
	public Record nextRecord() {
		if (iter == null || !iter.hasNext()) {
			this.advanceToNextLocation();
		}
		File next = iter.next();
		this.currentFile = next;
		invokeListeners(next);
		List<Writable> ret = loadFromFile(next);

		return new org.datavec.api.records.impl.Record(ret,
				new RecordMetaDataURI(next.toURI(), FileRecordReader.class));
	}

	private List<Writable> loadFromFile(File next) {
		List<Writable> ret = new ArrayList<>();
		try {
			String text = FileUtils.readFileToString(next);
			List<String> wordList = Arrays.asList(text.split(" ")).stream().map(word -> tokenizer.getTokenPreProcessor().preProcess(word)).filter(word -> dictionary.hasWord(word)).collect(Collectors.toList());
			if (wordList.isEmpty())
				return next();
			INDArray average = dictionary.getWordVectorsMean(wordList);

			for (int i = 0; i < average.columns(); i++)
				ret.add(new DoubleWritable(average.getRow(0).getDouble(i)));

			if (appendLabel) {
				ret.add(new IntWritable(labels.indexOf(next.getParentFile().getName())));
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		return ret;
	}
}
