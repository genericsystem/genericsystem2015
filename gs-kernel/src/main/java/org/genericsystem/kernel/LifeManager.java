package org.genericsystem.kernel;

import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LifeManager {

	public final static long TS_OLD_SYSTEM = 1L;
	public final static long TS_SYSTEM = 0L;
	public final static long[] SYSTEM_TS = new long[] { TS_SYSTEM, 0L, Long.MAX_VALUE };
	public final static long[] USER_TS = new long[] { Long.MAX_VALUE, 0L, Long.MAX_VALUE };

	protected static Logger log = LoggerFactory.getLogger(LifeManager.class);

	private long birthTs;
	private final AtomicLong lastReadTs;
	private long deathTs;
	private final ReentrantReadWriteLock lock = new ReentrantReadWriteLock();

	LifeManager(long[] otherTs) {
		this.birthTs = otherTs[0];
		this.lastReadTs = new AtomicLong(otherTs[1]);
		this.deathTs = otherTs[2];
	}

	// public void beginLifeIfNecessary(long birthTs) {
	// if (this.birthTs == Long.MAX_VALUE)
	// this.birthTs = birthTs;
	// else
	// assert this.birthTs < birthTs : "Generic is already marked as borned but later";
	// }

	public void beginLife(long birthTs) {
		// assert isWriteLockedByCurrentThread();
		assert this.birthTs == Long.MAX_VALUE || this.birthTs == 0L : "Generic is already born";
		if (this.birthTs == Long.MAX_VALUE)
			this.birthTs = birthTs;
	}

	void cancelBeginLife() {
		assert isWriteLockedByCurrentThread();
		birthTs = Long.MAX_VALUE;
	}

	public boolean isAlive(long contextTs) {
		if (contextTs < birthTs)
			return false;
		readLock();
		try {
			atomicAdjustLastReadTs(contextTs);
			return contextTs >= birthTs && contextTs < deathTs;
		} finally {
			readUnlock();
		}
	}

	public void checkMvcc(long contextTs) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		if (deathTs != Long.MAX_VALUE)
			throw new OptimisticLockConstraintViolationException("Attempt to kill a generic that is already killed by another thread");
		assert isWriteLockedByCurrentThread();
		if (contextTs < lastReadTs.get())
			throw new ConcurrencyControlException("" + contextTs + " " + lastReadTs.get());
	}

	public void kill(long contextTs) {
		// assert lock.isWriteLockedByCurrentThread();
		assert birthTs != 0L;
		assert contextTs >= birthTs : "Can not kill a generic that is not yet born";
		assert deathTs == Long.MAX_VALUE : "Can not kill a generic that will die in the future";
		assert contextTs >= getLastReadTs() : "Mvcc rule violation";
		atomicAdjustLastReadTs(contextTs);
		deathTs = contextTs;
	}

	void resurect() {
		assert isWriteLockedByCurrentThread();
		deathTs = Long.MAX_VALUE;
	}

	public long getLastReadTs() {
		return lastReadTs.get();
	}

	public long getDeathTs() {
		return deathTs;
	}

	public void atomicAdjustLastReadTs(long contextTs) {
		long current = lastReadTs.get();
		if (contextTs <= current)
			return;
		for (;;) {
			current = lastReadTs.get();
			if (lastReadTs.compareAndSet(current, contextTs))
				break;
		}
	}

	public void writeLock() {
		lock.writeLock().lock();
	}

	public void writeUnlock() {
		lock.writeLock().unlock();
	}

	public void readLock() {
		lock.readLock().lock();
	}

	public void readUnlock() {
		lock.readLock().unlock();
	}

	public boolean isWriteLockedByCurrentThread() {
		return lock.isWriteLockedByCurrentThread();
	}

	public long getBirthTs() {
		return birthTs;
	}

	public boolean willDie() {
		return deathTs != Long.MAX_VALUE;
	}

	public boolean isSystem() {
		return getBirthTs() == TS_SYSTEM;
	}

}