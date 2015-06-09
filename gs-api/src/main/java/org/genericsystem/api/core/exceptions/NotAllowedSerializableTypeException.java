package org.genericsystem.api.core.exceptions;

/**
 * Thrown when a <code>Serializable</code> type is not allowed. Only primitive types and <code>String</code> are allowed.
 * 
 * @author Nicolas Feybesse
 */
public class NotAllowedSerializableTypeException extends Exception {
	private static final long serialVersionUID = -7537013523266272175L;

	/**
	 * Constructs a <code>NotAllowedSerializableTypeException</code> with the specified detail message.
	 *
	 * @param message
	 *            the detail message.
	 */
	public NotAllowedSerializableTypeException(String message) {
		super(message);
	}
}
