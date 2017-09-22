package org.genericsystem.cv.model;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;

import javax.xml.bind.DatatypeConverter;

import org.apache.commons.io.FilenameUtils;
import org.genericsystem.cv.Zone;

/**
 * This class contains only static methods, which can be used as general purpose tools.
 * 
 * @author Pierrik Lassalas
 */
public class ModelTools {

	/**
	 * Default {@code pattern} for formatting the {@link LocalDateTime} objects.
	 */
	public static final String DATE_TIME_FORMAT = "uuuu-MM-dd HH:mm:ss";

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
	 * Generate a hash code from a file. Care must be taken to verify that the file exists before calling this method (otherwise an exception is thrown).
	 * 
	 * @param path - the {@link Path} of the file
	 * @param algorithm - the desired algorithm used to generate the hash code. Every implementation of the Java platform is required to support the following standard MessageDigest algorithms: MD5, SHA-1, SHA-256
	 * @return the computed {@code hash} as a hexadecimal String
	 * @throws RuntimeException when the specified algorithm is not found, or when the file could not be read
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
	 * Generate a hash code from an array of bytes.
	 * 
	 * @param bytes - an array of bytes
	 * @param algorithm - the desired algorithm used to generate the hash code. Every implementation of the Java platform is required to support the following standard MessageDigest algorithms: MD5, SHA-1, SHA-256
	 * @return the computed {@code hash} as a hexadecimal String
	 * @throws RuntimeException when the specified algorithm is not found
	 */
	public static String getHashFromBytes(byte[] bytes, String algorithm) {
		MessageDigest md;
		try {
			md = MessageDigest.getInstance(algorithm);
			md.update(bytes);
		} catch (NoSuchAlgorithmException e) {
			throw new RuntimeException("Unable to generate a hash code (no such algorithm)", e);
		}
		byte[] hash = md.digest();
		return DatatypeConverter.printHexBinary(hash);
	}

	/**
	 * Generate a simple hash from a file. Care must be taken to verify that the file exists before calling this method (otherwise an exception is thrown).
	 * 
	 * @param path - the {@link Path} of the file
	 * @return the computed {@code hash} as a hexadecimal String
	 * @throws RuntimeException when the specified file could not be read
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

	/**
	 * Return the current {@link LocalDateTime} as the number of milliseconds since Epoch.
	 * 
	 * @return a {@code Long} representing the local timestamp
	 */
	public static Long getCurrentDate() {
		LocalDateTime ldt = LocalDateTime.now();
		return ldt.atZone(ZoneOffset.systemDefault()).toInstant().toEpochMilli();
	}

	/**
	 * Convert a {@code String} into a {@link LocalDateTime} using default pattern.
	 * 
	 * @param date the date in a {@code String} format
	 * @return the corresponding date object
	 */
	public static LocalDateTime getCurrentDateFromString(String date) {
		return LocalDateTime.parse(date, DateTimeFormatter.ofPattern(DATE_TIME_FORMAT));
	}

	/**
	 * Convert a {@code String} into a {@link LocalDateTime} using a custom pattern.
	 * 
	 * @param date - the date in a {@code String} format
	 * @param pattern - the pattern to be used for conversion
	 * @return the corresponding date object
	 */
	public static LocalDateTime getCurrentDateFromString(String date, String pattern) {
		return LocalDateTime.parse(date, DateTimeFormatter.ofPattern(pattern));
	}

	/**
	 * Format a {@link LocalDateTime} using the default pattern.
	 * 
	 * @param timestamp - the timestamp to convert
	 * @return a {@code String} representation of the timestamp
	 */
	public static String formatDate(Long timestamp) {
		return formatDate(timestamp, DATE_TIME_FORMAT);
	}

	/**
	 * Format a {@link LocalDateTime} using a custom pattern.
	 * 
	 * @param timestamp - the timestamp to convert
	 * @param pattern - the pattern to be used for conversion
	 * @return a {@code String} representation of the timestamp
	 */
	public static String formatDate(Long timestamp, String pattern) {
		LocalDateTime ldt = LocalDateTime.ofInstant(Instant.ofEpochMilli(timestamp), ZoneOffset.systemDefault());
		return ldt.format(DateTimeFormatter.ofPattern(pattern));
	}

	/**
	 * Get the ImgClass name of a given image.
	 * 
	 * @param imag ePath - the Path of the image
	 * @return a String representing the ImgClass name
	 */
	public static String getImgClass(final Path imagePath) {
		final Path imgClassDirectory = imagePath.getParent();
		final String docType = imgClassDirectory.getName(imgClassDirectory.getNameCount() - 1).toString();
		return docType;
	}

	/**
	 * Generate a filename from a given file, using a SHA-256 string computed from the file's bytes.
	 * 
	 * @param filePath - the {@link Path} to the file
	 * @return a String representing the SHA-256 hashcode + the file extension
	 */
	public static String generateFileName(Path filePath) {
		try {
			String filename = ModelTools.getHashFromFile(filePath, "sha-256");
			String filenameExt = filename + "." + FilenameUtils.getExtension(filePath.getFileName().toString());
			return filenameExt;
		} catch (RuntimeException e) {
			throw new RuntimeException("An error has occured during the generation of the hashcode from file", e);
		}
	}

	/**
	 * Generate a unique ID for a given {@link Zone}, using a SHA-256 string computed from the zone's rectangle. This hashcode is expected to be unique for a given {@link Zone}.
	 * 
	 * @param zone - the zone for which a label has to be generated
	 * @return a String representing the zone's unique ID
	 */
	public static String generateZoneUID(Zone zone) {
		try {
			byte[] bytes = zone.getRect().toString().getBytes(Charset.forName("UTF8"));
			String zoneUID = ModelTools.getHashFromBytes(bytes, "sha-256");
			return zoneUID;
		} catch (RuntimeException e) {
			throw new RuntimeException("An error has occured during the generation of the hashcode from zone", e);
		}
	}

}
