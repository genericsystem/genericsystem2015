package org.genericsystem.cv.model;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

import javax.xml.bind.DatatypeConverter;

/**
 * This class contains only static methods, which can be used as general purpose
 * tools.
 * 
 * @author Pierrik Lassalas
 *
 */
public class ModelTools {

	public static void main(String[] args) {
		Path path = Paths.get(System.getProperty("user.home"), "Downloads", "photosafpa.zip");
		File file = path.toFile();
		System.out.println(file.getAbsolutePath());
		if (!file.exists()) {
			System.out.println("File does not exists");
			System.exit(0);
		}
		String hash = hashCode(path);
		String md5 = getHashFromFile(path, "md5");
		String sha1 = getHashFromFile(path, "sha-1");
		String sha256 = getHashFromFile(path, "sha-256");
		String sha512 = getHashFromFile(path, "sha-512");

		System.out.println("hash: " + hash + " - " + hash.length());
		System.out.println("md5: " + md5 + " - " + md5.length());
		System.out.println("sha-1: " + sha1 + " - " + sha1.length());
		System.out.println("sha-256: " + sha256 + " - " + sha256.length());
		System.out.println("sha-512: " + sha512 + " - " + sha512.length());
	}

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
			md.update(Files.readAllBytes(path));
		} catch (NoSuchAlgorithmException e) {
			throw new RuntimeException("Unable to generate a hash code (no such algorithm)", e);
		} catch (IOException e) {
			throw new RuntimeException("Unable to generate a hash code (problem reading the file)", e);
		}
		byte[] hash = md.digest();
		return DatatypeConverter.printHexBinary(hash);
	}

	/**
	 * Generates a simple hash from a file.
	 * 
	 * Care must be taken to verify that the file exists before calling this
	 * method (otherwise an exception is thrown).
	 * 
	 * @param path
	 *            - the {@link Path} of the file
	 * @return the computed {@code hash} as a hexadecimal String
	 * @throws RuntimeException
	 *             when the specified file could not be read
	 */
	private static String hashCode(Path path) throws RuntimeException {
		int hashCode;
		try {
			hashCode = Arrays.hashCode(Files.readAllBytes(path));
		} catch (IOException e) {
			throw new RuntimeException("Unable to generate a hash code (problem reading the file", e);
		}
		return Integer.toHexString(hashCode).toUpperCase();
	}
}
