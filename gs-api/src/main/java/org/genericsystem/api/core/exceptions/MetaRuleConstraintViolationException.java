package org.genericsystem.api.core.exceptions;

/**
 * Thrown when a component on a link doesn't inherit from a super component.
 * 
 * @author Nicolas Feybesse
 */
public class MetaRuleConstraintViolationException extends ConstraintViolationException {
	private static final long serialVersionUID = 8833957941413414352L;

	/**
	 * Constructs a <code>MetaRuleConstraintViolationException</code> with the specified detail message.
	 *
	 * @param message
	 *            the detail message.
	 */
	public MetaRuleConstraintViolationException(String message) {
		super(message);
	}
}
