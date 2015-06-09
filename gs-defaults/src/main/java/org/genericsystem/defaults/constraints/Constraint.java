package org.genericsystem.defaults.constraints;

import java.io.Serializable;

import org.genericsystem.api.core.IVertex.SystemProperty;
import org.genericsystem.api.core.exceptions.ConstraintViolationException;
import org.genericsystem.defaults.DefaultVertex;

/**
 * Represents a constraint in Generic System. A constraint is special form of system property which constrains Generic System.
 * 
 * @author Nicolas Feybesse
 *
 * @param <T>
 *            the implementation of DefaultVertex.
 */
public interface Constraint<T extends DefaultVertex<T>> extends SystemProperty {
	/**
	 * Checks this constraint.
	 * 
	 * @param modified
	 *            the Generic for which check the constraint.
	 * @param constraintBase
	 *            the Generic where is the constraint.
	 * @param value
	 *            the value of the constraint.
	 * @param axe
	 *            the axe of the constraint.
	 * @param isOnAdd
	 *            <code>true</code> if the constraint should be checked when adding a Generic, <code>false</code> otherwise.
	 * @param isFlushTime
	 *            <code>true</code> if the constraint should be checked when flushing the cache, <code>false</code> otherwise.
	 * @param isRevert
	 *            <code>true</code> if the constraint should be checked from the target, <code>false</code> otherwise (should be checked from the base).
	 * @throws ConstraintViolationException
	 *             if the constraint can not be checked.
	 */
	void check(T modified, T constraintBase, Serializable value, int axe, boolean isOnAdd, boolean isFlushTime, boolean isRevert) throws ConstraintViolationException;

	/**
	 * Represents a constraint which is axed and checked.
	 *
	 * @param <T>
	 *            the implementation of DefaultVertex.
	 */
	public static interface AxedCheckedConstraint<T extends DefaultVertex<T>> extends Constraint<T> {
		/**
		 * Checks this constraint.
		 * 
		 * @param modified
		 *            the Generic for which check the constraint.
		 * @param constraintBase
		 *            the Generic where is the constraint.
		 * @param value
		 *            the value of the constraint.
		 * @param axe
		 *            the axe of the constraint.
		 * @param isRevert
		 *            <code>true</code> if the constraint should be checked from the target, <code>false</code> otherwise (should be checked from the base).
		 * @throws ConstraintViolationException
		 *             if the constraint can not be checked.
		 */
		void check(T modified, T constraintBase, Serializable value, int axe, boolean isRevert) throws ConstraintViolationException;

		@Override
		default void check(T modified, T constraintBase, Serializable value, int axe, boolean isOnAdd, boolean isFlushTime, boolean isRevert) throws ConstraintViolationException {
			check(modified, constraintBase, value, axe, isRevert);
		}
	}

	/**
	 * Represents a constraint which is <em>NOT</em> axed and which is checked.
	 *
	 * @param <T>
	 *            the implementation of DefaultVertex.
	 */
	public static interface CheckedConstraint<T extends DefaultVertex<T>> extends AxedCheckedConstraint<T> {
		/**
		 * Checks this constraint.
		 * 
		 * @param modified
		 *            the Generic for which check the constraint.
		 * @param constraintBase
		 *            the Generic where is the constraint.
		 * @param value
		 *            the value of the constraint.
		 * @throws ConstraintViolationException
		 *             if the constraint can not be checked.
		 */
		void check(T modified, T constraintBase, Serializable value) throws ConstraintViolationException;

		@Override
		default void check(T modified, T constraintBase, Serializable value, int axe, boolean isRevert) throws ConstraintViolationException {
			check(modified, constraintBase, value);
		}
	}

	/**
	 * Represents a constraint which is axed and which can be checked.
	 *
	 * @param <T>
	 *            the implementation of DefaultVertex.
	 */
	public static interface AxedCheckableConstraint<T extends DefaultVertex<T>> extends Constraint<T> {
		/**
		 * Checks this constraint.
		 * 
		 * @param modified
		 *            the Generic for which check the constraint.
		 * @param constraintBase
		 *            the Generic where is the constraint.
		 * @param value
		 *            the value of the constraint.
		 * @param axe
		 *            the axe of the constraint.
		 * @param isRevert
		 *            <code>true</code> if the constraint should be checked from the target, <code>false</code> otherwise (should be checked from the base).
		 * @throws ConstraintViolationException
		 *             if the constraint can not be checked.
		 */
		void check(T modified, T constraintBase, Serializable value, int axe, boolean isRevert) throws ConstraintViolationException;

		/**
		 * Indicates whether the constraint can be checked.
		 * 
		 * @param modified
		 *            the Generic for which check the constraint.
		 * @param isOnAdd
		 *            <code>true</code> if the constraint should be checked when adding a Generic, <code>false</code> otherwise.
		 * @param isFlushTime
		 *            <code>true</code> if the constraint should be checked when flushing the cache, <code>false</code> otherwise.
		 * @param isRevert
		 *            <code>true</code> if the constraint should be checked from the target, <code>false</code> otherwise (should be checked from the base).
		 * @return <code>true</code> if the constraint can be checked, <code>false</code> otherwise.
		 */
		boolean isCheckable(T modified, boolean isOnAdd, boolean isFlushTime, boolean isRevert);

		@Override
		default void check(T modified, T constraintBase, Serializable value, int axe, boolean isOnAdd, boolean isFlushTime, boolean isRevert) throws ConstraintViolationException {
			if (isCheckable(modified, isOnAdd, isFlushTime, isRevert))
				check(modified, constraintBase, value, axe, isRevert);
		}

	}

	/**
	 * Represents a constraint which is <em>NOT</em> axed and which can be checked.
	 *
	 * @param <T>
	 *            the implementation of DefaultVertex.
	 */
	public static interface CheckableConstraint<T extends DefaultVertex<T>> extends AxedCheckableConstraint<T> {
		/**
		 * Checks this constraint.
		 * 
		 * @param modified
		 *            the Generic for which check the constraint.
		 * @param constraintBase
		 *            the Generic where is the constraint.
		 * @param value
		 *            the value of the constraint.
		 * @throws ConstraintViolationException
		 *             if the constraint can not be checked.
		 */
		void check(T modified, T constraintBase, Serializable value) throws ConstraintViolationException;

		@Override
		default void check(T modified, T constraintBase, Serializable value, int axe, boolean isRevert) throws ConstraintViolationException {
			check(modified, constraintBase, value);
		}
	}
}
