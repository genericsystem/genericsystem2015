package org.genericsystem.mutability;

import java.util.function.Supplier;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class AbstractTest {

	protected static Logger log = LoggerFactory.getLogger(AbstractTest.class);

	@FunctionalInterface
	public static interface VoidSupplier {
		public void getNothing();
	}

	public void catchAndCheckCause(VoidSupplier supplier, Class<? extends Throwable> clazz) {
		try {
			supplier.getNothing();
		} catch (RollbackException ex) {
			if (ex.getCause() == null)
				throw new IllegalStateException("Rollback Exception has not any cause", ex);
			if (!clazz.isAssignableFrom(ex.getCause().getClass()))
				throw new IllegalStateException("Cause of rollback exception is not of type : " + clazz.getSimpleName() + ", but is " + ex.getCause(), ex);
			return;
		}
		assert false : "Unable to catch any rollback exception!";
	}

	public void catchAndCheckCause(Supplier<?> supplier, Class<? extends Throwable> clazz) {
		try {
			supplier.get();
		} catch (RollbackException ex) {
			if (ex.getCause() == null)
				throw new IllegalStateException("Rollback Exception has not any cause", ex);
			if (!clazz.isAssignableFrom(ex.getCause().getClass()))
				throw new IllegalStateException("Cause of rollback exception is not of type : " + clazz.getSimpleName() + ", but is " + ex.getCause(), ex);
			return;
		}
		assert false : "Unable to catch any rollback exception!";
	}
}
