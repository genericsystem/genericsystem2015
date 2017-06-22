package org.genericsystem.cv.comparator;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.stream.Stream;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.Scores;
import org.genericsystem.cv.Zone;

public class ZoneScorer2 {

	private FileWriter writer;
	private static final String basePath = "classes/id-fr-front/csv/";
	private static final String delimiter = " ";
	
	private final Scores scores = new Scores();
	private final Zone zone;

	public ZoneScorer2(Zone zone, Stream<Img> imgs, File file) {
		this.zone = zone;
		try {
			writer = new FileWriter(basePath + file.getName().replaceAll(".png", "") + "-untreated.csv", true);
//			writer = new FileWriter(basePath + file.getName().replaceAll(".png", "") + "-eqhisto.csv", true);
//			writer = new FileWriter(basePath + file.getName().replaceAll(".png", "") + "-eqhistoadapt.csv", true);

			log(writer, "Zone", Integer.toString(zone.getNum()));
			
			writer.append("OCRText(untreated)").append(delimiter);
			// Untreated images
			imgs.forEach(img -> {
				img = img.equalizeHistoAdaptative();
				String s = zone.ocr(img);
				s = s.replace("\n", "").replace(" ", "").replaceAll("\t", "").trim();
				try {
					writer.append(s);
					writer.append(delimiter);
				} catch (IOException e) { 
					e.printStackTrace();
				}
				scores.put(s);
			});
			writer.append("\n");
			log(writer, "bestText", this.getBestText());
			log(writer, "bestText2", this.getBestText2());
			
			writer.flush();
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public ZoneScorer2(Zone zone, Stream<File> files) {
		this.zone = zone;
		// TODO Auto-generated constructor stub
		
	}

	private void log(FileWriter writer, String...strings) throws IOException{
		for(String s : strings){
			writer.append(s);
			writer.append(delimiter);
		}
		writer.append("\n");
	}

	public Zone getZone() {
		return zone;
	}

	public double getBestScore() {
		return scores.getBestScore();
	}

	public String getBestText() {
		return scores.getBestText();
	}

	public String getBestText2() {
		return scores.getBestText2();
	}

}
