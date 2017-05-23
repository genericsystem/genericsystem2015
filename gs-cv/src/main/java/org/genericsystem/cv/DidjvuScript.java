package org.genericsystem.cv;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DidjvuScript {

	private static Logger log = LoggerFactory.getLogger(DidjvuScript.class);
	public static final String shellScript = "../gs-cv/didjvu.sh";

	public static void main(String[] args) {
		try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(Paths.get(".", "classes"), Files::isDirectory)) {
			for (Path path : directoryStream) {
				runDidjvuScript(path.toString());
			}
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
	}

	public static int runDidjvuScript(String parameter) {
		try {
			Process process = Runtime.getRuntime().exec(new String[] { shellScript, parameter });
			System.out.println("Didjvu processing " + parameter);
			return process.waitFor();
		} catch (IOException | InterruptedException e) {
			log.warn("Shell script execution failed on " + parameter + ".");
			return 1;
		}
	}
}
