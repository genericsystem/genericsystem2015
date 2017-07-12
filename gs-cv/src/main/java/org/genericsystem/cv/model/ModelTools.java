package org.genericsystem.cv.model;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import javax.xml.bind.DatatypeConverter;

/**
 * This class contains only static methods, which can be used as general purpose
 * tools.
 * 
 * @author Pierrik Lassalas
 *
 */
public class ModelTools {

	/**
	 * Generates a hash code from a file.
	 * 
	 * Care must be taken to verify that the file exists before calling this
	 * method (otherwise an exception is thrown).
	 * 
	 * @param path
	 *            - the {@link Path} of the file
	 * @param algorithm
	 *            - the desired algorithm used to generate the has code. Every
	 *            implementation of the Java platform is required to support the
	 *            following standard MessageDigest algorithms: MD5, SHA-1,
	 *            SHA-256
	 * @return the computed {@code hash} as a hexadecimal String
	 * @throws RuntimeException
	 *             when the specified algorithm is not found, or when the file
	 *             could not be read
	 */
	public static String getHashFromFile(Path path, String algorithm) throws RuntimeException {
		MessageDigest md;
		try {
			md = MessageDigest.getInstance(algorithm);
		} catch (NoSuchAlgorithmException e) {
			throw new RuntimeException("Unable to generate a hash code (no such algorithm)", e);
		}

		long start = System.nanoTime();
		try {
			md.update(Files.readAllBytes(path));
		} catch (IOException e) {
			throw new RuntimeException("Unable to generate a hash code (problem reading the file)", e);
		}
		byte[] hash = md.digest();
		long stop = System.nanoTime();

		System.out.println(">>> " + algorithm);
		System.out.println("Computation time: " + (stop - start) / 1_000_000 + "ms");
		return DatatypeConverter.printHexBinary(hash);
	}
}
