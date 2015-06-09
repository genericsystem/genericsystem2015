package org.genericsystem.api.core.exceptions;

/**
 * Thrown when the level of a Generic is inappropriate.<br>
 * For example, trying to instantiate an instance of an instance will throw a LevelConstraintViolationException.
 * 
 * @author Nicolas Feybesse
 */
public class LevelConstraintViolationException extends ConstraintViolationException {
	private static final long serialVersionUID = -744884478985009850L;

	/**
	 * Constructs a <code>LevelConstraintViolationException</code> with the specified detail message.
	 *
	 * @param message
	 *            the detail message.
	 */
	public LevelConstraintViolationException(String message) {
		super(message);
	}
}
