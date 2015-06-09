package org.genericsystem.api.core.exceptions;

/**
 * Thrown when an operation of the engine on a Generic is ambiguous.<br>
 * For example, suppose we have a <code>vehicle</code> with an attribute <code>power</code>. Then we create a subtype of vehicle called <code>car</code> with a first attribute called <code>carPower</code> which overrides <code>power</code> and a second
 * attribute called <code>carPower2</code> which overrides <code>power</code> too. Then we create an instance of <code>car</code> called <code>myCar</code>. Finally we try to add an holder for <code>power</code> on <code>myCar</code>. An
 * AmbiguousSelectionException will be thrown as the system could not determinate if the holder is an instance of <code>carPower</code> or <code>carPower2</code>.
 * 
 * @author Nicolas Feybesse
 */
public class AmbiguousSelectionException extends ConstraintViolationException {
	private static final long serialVersionUID = 1L;

	/**
	 * Constructs an <code>AmbiguousSelectionException</code> with the specified detail message.
	 *
	 * @param message
	 *            the detail message.
	 */
	public AmbiguousSelectionException(String message) {
		super(message);
	}
}
