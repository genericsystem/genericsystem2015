package org.genericsystem.cache;

import io.netty.buffer.ByteBuf;
import io.netty.util.CharsetUtil;
import io.vertx.core.buffer.Buffer;

import java.io.Serializable;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.List;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.AxedPropertyClass;
import org.genericsystem.common.Vertex;

public class GSBuffer implements Buffer {
	private int index = 0;
	private final Buffer internal;

	public GSBuffer(Buffer internal) {
		this.internal = internal;
	}

	public void writeToBuffer(Buffer buffer) {
		internal.writeToBuffer(buffer);
	}

	public int readFromBuffer(int pos, Buffer buffer) {
		return internal.readFromBuffer(pos, buffer);
	}

	public String toString(String enc) {
		return internal.toString(enc);
	}

	public byte getByte(int pos) {
		return internal.getByte(pos);
	}

	public int getInt(int pos) {
		return internal.getInt(pos);
	}

	public long getLong(int pos) {
		return internal.getLong(pos);
	}

	public double getDouble(int pos) {
		return internal.getDouble(pos);
	}

	public float getFloat(int pos) {
		return internal.getFloat(pos);
	}

	public short getShort(int pos) {
		return internal.getShort(pos);
	}

	public byte[] getBytes() {
		return internal.getBytes();
	}

	public byte[] getBytes(int start, int end) {
		return internal.getBytes(start, end);
	}

	public Buffer getBuffer(int start, int end) {
		return internal.getBuffer(start, end);
	}

	public String getString(int start, int end, String enc) {
		return internal.getString(start, end, enc);
	}

	public String getString(int start, int end) {
		return internal.getString(start, end);
	}

	public Buffer appendBuffer(Buffer buff) {
		return internal.appendBuffer(buff);
	}

	public Buffer appendBuffer(Buffer buff, int offset, int len) {
		return internal.appendBuffer(buff, offset, len);
	}

	public Buffer appendBytes(byte[] bytes) {
		return internal.appendBytes(bytes);
	}

	public Buffer appendBytes(byte[] bytes, int offset, int len) {
		return internal.appendBytes(bytes, offset, len);
	}

	public Buffer appendByte(byte b) {
		return internal.appendByte(b);
	}

	public Buffer appendInt(int i) {
		return internal.appendInt(i);
	}

	public Buffer appendLong(long l) {
		return internal.appendLong(l);
	}

	public Buffer appendShort(short s) {
		return internal.appendShort(s);
	}

	public Buffer appendFloat(float f) {
		return internal.appendFloat(f);
	}

	public Buffer appendDouble(double d) {
		return internal.appendDouble(d);
	}

	public Buffer appendString(String str, String enc) {
		return internal.appendString(str, enc);
	}

	public Buffer appendString(String str) {
		return internal.appendString(str);
	}

	public Buffer appendGSString(String string) {
		return appendGSBytes(string.getBytes(CharsetUtil.UTF_8));

	}

	public Buffer appendGSVertex(Vertex vertex) {
		appendGSClazz(vertex.getClazz());
		appendLong(vertex.getTs());
		appendLong(vertex.getMeta());
		appendGSLongList(vertex.getSupers());
		appendGSValue(vertex.getValue());
		appendGSLongList(vertex.getComponents());
		appendGSLongArray(vertex.getOtherTs());
		return this;

	}

	public Buffer appendGSVertexArray(Vertex[] vertexArray) {
		appendInt(vertexArray.length);
		for (Vertex v : vertexArray)
			appendGSVertex(v);
		return this;
	}

	public Buffer appendGSLongList(List<Long> array) {
		appendInt(array.size());
		for (long l : array)
			appendLong(l);
		return this;
	}

	public Buffer appendGSLongArray(long[] array) {
		appendInt(array.length);
		for (long l : array)
			appendLong(l);
		return this;
	}

	public Buffer appendGSValue(Serializable value) {
		for (Entry<Integer, Class<?>> entry : ApiStatics.SUPPORTED_VALUE_CLASSES
				.entrySet()) {
			if (entry.getValue().isInstance(value)) {
				appendInt(entry.getKey());

				switch (entry.getKey()) {
				case 0: {
					appendGSClazz(((AxedPropertyClass) value).getClazz());
					appendInt(((AxedPropertyClass) value).getAxe());
					return this;
				}
				case 1: {
					appendInt(((Boolean) value).booleanValue() ? 1 : 0);
					return this;
				}
				case 2: {
					appendGSBytes((byte[]) value);
					return this;
				}
				case 3: {
					appendDouble((Double) value);
					return this;
				}
				case 4: {
					appendFloat((Float) value);
					return this;
				}
				case 5: {
					appendInt((Integer) value);
					return this;
				}
				case 6: {
					appendLong((Long) value);
					return this;
				}
				case 7: {
					appendShort((Short) value);
					return this;
				}
				case 8: {
					appendGSString((String) value);
					return this;
				}
				case 9: {
					appendGSClazz((Class<?>) value);
					return this;
				}
				default:
					throw new IllegalStateException(
							"unknowned class code in appendGSValue"
									+ entry.getKey());
				}
			}
		}
		return this;
	}

	public Buffer appendGSBytes(byte[] bytes) {
		appendInt(bytes.length);
		appendBytes(bytes);
		return this;
	}

	public Buffer appendGSClazz(Class<?> clazz) {
		appendGSString(clazz != null ? clazz.getName() : "");
		return this;
	}

	public int getInt() {
		int result = getInt(index);
		index += 4;
		return result;
	}

	public long getLong() {
		long result = getLong(index);
		index += 8;
		return result;
	}

	public double getDouble() {
		double result = getDouble(index);
		index += 16;
		return result;
	}

	public float getFloat() {
		float result = getFloat(index);
		index += 8;
		return result;
	}

	public short getShort() {
		short result = getShort(index);
		index += 2;
		return result;
	}

	public String getGSString() {
		byte[] result = getGSBytes();
		return new String(result);

	}

	public Vertex getGSVertex() {
		Class<?> clazz = getGSClazz();
		long ts = getLong();
		long meta = getLong();
		List<Long> supers = Arrays.stream(getGSLongArray()).mapToObj(l -> l)
				.collect(Collectors.toList());
		Serializable value = getGSValue();
		List<Long> components = Arrays.stream(getGSLongArray())
				.mapToObj(l -> l).collect(Collectors.toList());
		long[] otherTs = getGSLongArray();
		return new Vertex(clazz, ts, meta, supers, value, components, otherTs);
	}

	public Vertex[] getGSVertexArray() {
		Vertex[] result = new Vertex[getInt()];
		for (int i = 0; i < result.length; i++)
			result[i] = getGSVertex();
		return result;
	}

	public long[] getGSLongArray() {
		long[] result = new long[getInt()];
		for (int i = 0; i < result.length; i++)
			result[i] = getLong();
		return result;
	}

	public Serializable getGSValue() {
		int code = getInt();
		switch (code) {
		case 0:
			return new AxedPropertyClass((Class) getGSClazz(), getInt());
		case 1:
			return (boolean) (getInt() == 1);
		case 2:
			return getGSBytes();
		case 3:
			return getDouble();
		case 4:
			return getFloat();
		case 5:
			return getInt();
		case 6:
			return getLong();
		case 7:
			return getShort();
		case 8:
			return getGSString();
		case 9:
			return getGSClazz();
		default:
			throw new IllegalStateException("unknowned class code");
		}
	}

	public Class<?> getGSClazz() {
		String string = getGSString();
		try {

			return string.isEmpty() ? null : Class.forName(string);
		} catch (ClassNotFoundException e) {
			throw new IllegalStateException(e);
		}
	}

	public byte[] getGSBytes() {
		int length = getInt();
		byte[] result = getBytes(index, index + length);
		if (result.length != length)
			throw new IllegalStateException(result.length + " " + length);
		index += length;
		return result;
	}

	public byte getByte() {
		byte result = getByte(index);
		index++;
		return result;
	}

	public Buffer setByte(int pos, byte b) {
		return internal.setByte(pos, b);
	}

	public Buffer setInt(int pos, int i) {
		return internal.setInt(pos, i);
	}

	public Buffer setLong(int pos, long l) {
		return internal.setLong(pos, l);
	}

	public Buffer setDouble(int pos, double d) {
		return internal.setDouble(pos, d);
	}

	public Buffer setFloat(int pos, float f) {
		return internal.setFloat(pos, f);
	}

	public Buffer setShort(int pos, short s) {
		return internal.setShort(pos, s);
	}

	public Buffer setBuffer(int pos, Buffer b) {
		return internal.setBuffer(pos, b);
	}

	public Buffer setBuffer(int pos, Buffer b, int offset, int len) {
		return internal.setBuffer(pos, b, offset, len);
	}

	public Buffer setBytes(int pos, ByteBuffer b) {
		return internal.setBytes(pos, b);
	}

	public Buffer setBytes(int pos, byte[] b) {
		return internal.setBytes(pos, b);
	}

	public Buffer setBytes(int pos, byte[] b, int offset, int len) {
		return internal.setBytes(pos, b, offset, len);
	}

	public Buffer setString(int pos, String str) {
		return internal.setString(pos, str);
	}

	public Buffer setString(int pos, String str, String enc) {
		return internal.setString(pos, str, enc);
	}

	public int length() {
		return internal.length();
	}

	public Buffer copy() {
		return internal.copy();
	}

	public Buffer slice() {
		return internal.slice();
	}

	public Buffer slice(int start, int end) {
		return internal.slice(start, end);
	}

	public ByteBuf getByteBuf() {
		return internal.getByteBuf();
	}

}