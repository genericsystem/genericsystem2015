package org.genericsystem.api.core.exceptions;

/**
 * Thrown when an operation is performed on the engine but no cache is actually started.
 * 
 * @author Nicolas Feybesse
 */
public class CacheNoStartedException extends Exception {
	private static final long serialVersionUID = -3512365559694054465L;

	/**
	 * Constructs a <code>CacheNoStartedException</code> with the specified detail message.
	 *
	 * @param message
	 *            the detail message.
	 */
	public CacheNoStartedException(String message) {
		super(message);
	}
}
