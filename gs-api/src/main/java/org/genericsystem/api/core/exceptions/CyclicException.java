package org.genericsystem.api.core.exceptions;

/**
 * Thrown if your trying create a cylce.<br>
 * If you create a attribute placed on him even.
 * 
 * @author Nicolas Feybesse
 */
public class CyclicException extends ConstraintViolationException {

	private static final long serialVersionUID = -7948846771732496528L;

	/**
	 * Constructs an <code>CyclicException</code> with the specified detail message.
	 *
	 * @param message
	 *            the detail message.
	 */
	public CyclicException(String message) {
		super(message);
	}
}
