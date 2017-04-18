package org.genericsystem.cv;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.rendering.ImageType;
import org.apache.pdfbox.rendering.PDFRenderer;
import org.apache.pdfbox.tools.imageio.ImageIOUtil;

public class PdfToPngConverter {

	private final static String pdfDirectoryPath = "pdf";
	private final static String pngDirectoryPath = "png";

	public static void main(String[] args) {
		convertPdfToPng(pdfDirectoryPath, pngDirectoryPath);

	}

	static void convertPdfToPng(String sourceDirectoryName, String destinationDirectoryName) {
		File destinationDirectory = new File(destinationDirectoryName);
		if (!destinationDirectory.exists())
			destinationDirectory.mkdir();
		for (File image : new File(sourceDirectoryName).listFiles())
			if (image.getName().endsWith(".pdf"))
				convertPdfToImages(image, destinationDirectory);
	}

	private static void convertPdfToImages(File pdfFile, File destinationDirectory) {
		try {
			PDDocument document = PDDocument.load(new FileInputStream(pdfFile));
			PDFRenderer pdfRenderer = new PDFRenderer(document);
			String fileName = pdfFile.getName().replace(".pdf", "");

			int pageCounter = 0;
			for (PDPage page : document.getPages()) {
				BufferedImage bim = pdfRenderer.renderImageWithDPI(pageCounter, 300, ImageType.RGB);
				ImageIOUtil.writeImage(bim, destinationDirectory + "/" + fileName + "-" + (pageCounter++) + ".png", 300);
			}
			document.close();
			System.out.println("Converted Images are saved at -> " + destinationDirectory.getAbsolutePath());

		} catch (

		Exception e) {
			throw new RuntimeException(e);
		}
	}
}