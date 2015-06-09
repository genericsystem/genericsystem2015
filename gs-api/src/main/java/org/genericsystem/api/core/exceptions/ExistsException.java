package org.genericsystem.api.core.exceptions;

/**
 * Thrown when a Generic will be created but already exists on the engine.
 * 
 * @author Nicolas Feybesse
 */
public class ExistsException extends Exception {
	private static final long serialVersionUID = -4631985293285253439L;

	/**
	 * Constructs an <code>ExistsException</code> with the specified detail message.
	 *
	 * @param message
	 *            the detail message.
	 */
	public ExistsException(String message) {
		super(message);
	}
}
