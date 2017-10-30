package org.genericsystem.cv;

import org.opencv.core.CvType;
import org.opencv.core.Mat;

public class Matrix {

	private int m;
	private int n;
	private double[][] A;
//	private Mat openCVmat;

	public Matrix(int i, int j) {
		this.m = i;
		this.n = j;
		A = new double[i][j];
	}

	public Matrix(double[][] arrayDouble) {
		A = arrayDouble;
		n = arrayDouble.length;
		m = arrayDouble[0].length;
	}

	public Matrix() {
	
	}

	public void set(int i, int j, double value) {
		A[i][j] = value;
	}

	/**
	 * Realise the product of matrices
	 * 
	 */
	public Matrix times(Matrix matrix, double alpha) {

		if(matrix==null)
			throw new IllegalArgumentException("matrix is null");
		if (n != matrix.getm()) {
			throw new IllegalArgumentException("Matrix inner dimensions must agree. (" + m + "," + n + ")" + " vs (" + (matrix.getm() + "," + matrix.getn() + ")"));
		}

		Matrix localMatrix = new Matrix(m, matrix.getn());
		
		for (int i = 0; i < m; i++) {
			for (int j = 0; j < matrix.getn(); j++) {			
				
				double d = 0.0D;
				for (int k = 0; k < matrix.getm(); k++) {
					d += A[i][k] * matrix.get(k, j);
				}
				localMatrix.set(i, j, d*alpha);
			}
		}
		return localMatrix;

	}

	public double[] get(int j) {
		return A[j];
	}

	public double get(int i, int j) {
		return A[i][j];
	}

	public int getm() {
		return m;
	}

	public int getn() {
		return n;
	}

	public double[][] getA() {
		return A;
	}

	/**
	 * Calculate the cross-product of given matrices. Returns the computed matrix.
	 * 
	 * @param mat1
	 * @param mat2
	 * @return
	 */
	public static Matrix crossProduct(Matrix mat1, Matrix mat2) {

		if (mat1.getm() != 3 && mat2.getm() != 3 && mat1.getn() != 1 && mat2.getn() != 1) {
			throw new IllegalArgumentException("matrix dimension must be (3,1)");
		}

		Matrix array = new Matrix(3,1);
		array.set(0, 0, mat1.get(1, 0) * mat2.get(2, 0) - mat1.get(2, 0) * mat2.get(1, 0));
		array.set(1, 0, mat1.get(2, 0) * mat2.get(0, 0) - mat1.get(0, 0) * mat2.get(2, 0));
		array.set(2, 0, mat1.get(0, 0) * mat2.get(1, 0) - mat1.get(1, 0) * mat2.get(0, 0));
		return array;
		
	}

	/**
	 * Normalization of Matrix: each column member is divided by the sqrt of squares sum of that column members. For matrix (a,b,c) squares sum is s=(a**2 + b**2 + c**2). So nomalized matrix would be (a/sqrt(s), b/sqrt(s), c/sqrt(s))
	 */
	public Matrix normalize() {

//		for (int i = 0; i < n; i++) {
//			
//			double d = 0.0D;
//			for (int j = 0; j < m; j++) {
//				d += A[j][i]*A[j][i];
//			}
		double d = this.norm();
			if(d == 0.0D)
				throw new IllegalStateException("norm is 0, cannot normalize");
			for (int i = 0; i < n; i++) {
				for (int j = 0; j < m; j++) {
					A[j][i] = A[j][i] / d;
				}
			}
		return this;
	}
	
	
	/**
	 * inverse the matrix. openCV inversion implementation is used so matrix needs to be converted first, inversed, then converted back 
	 * 
	 * @return
	 */
	public Matrix inv(){		
		return convert(this.convert().inv());		
	}
	
/**
 * transpose the matrix
 * 
 * @return
 */

//	public Matrix t() {
//
//		    double[][] arrayOfDouble = new double[m][n];
//
//		    for (int i = 0; i < n; i++) {
//		      for (int j = 0; j < m; j++) {
//		        arrayOfDouble[j][i] = A[i][j];
//		      }
//		    }
//		    return new Matrix(arrayOfDouble);
//	}
	
	public Matrix t() {
		
		 Matrix localMatrix = new Matrix(n, m);
		    double[][] arrayOfDouble = localMatrix.getA();
		    for (int i = 0; i < m; i++) {
		      for (int j = 0; j < n; j++) {
		        arrayOfDouble[j][i] = A[i][j];
		      }
		    }
		    return localMatrix;
	}

	
	/**
	 * convert org.genericsystem.cv.Matrix to org.opencv.core.Mat
	 * 
	 * @return
	 */
	public Mat convert() {
		
		Mat convertedMat = new Mat(m,n,CvType.CV_64F);
		for(int i = 0; i<m; i++){
			for(int j = 0; j<n;j++){
				convertedMat.put(i, j, A[i][j]);
			}
		}		
		return convertedMat;
	}

	/**
	 * convert org.opencv.core.Mat to org.genericsystem.cv.Matrix   
	 * 
	 * @return
	 */
	public static Matrix convert(Mat mat) {
		
		Matrix matrix = new Matrix(mat.rows(), mat.cols());
		for(int i = 0; i<mat.rows(); i++){
			for(int j = 0; j<mat.cols();j++){
				matrix.set(i, j, mat.get(i, j)[0]);
			}
		}	
		return matrix;
	}

	/**
	 * performs the dot product of two vectors
	 * 
	 * @param matrix
	 * @return
	 */
	public double dot(Matrix matrix) {
		if (m != matrix.getm()  && n != matrix.getn()) {
			throw new IllegalArgumentException("vectors dimensions must be the same");
		}
		double d = 0.0D;
		for(int i =0;i<m;i++){
			for(int j=0;j<n;j++){
				d+=A[i][j]*matrix.get(i, j);
			}
		}		
		return d;
	}

	/**
	 * Calculate the norm (norm type = L2) of the matrix
	 * @return
	 */
	public double norm() {
		
		double d = 0.0D;
		for (int i = 0; i < n; i++) {			
			for (int j = 0; j < m; j++) {
				d += A[j][i]*A[j][i];
			}
		}
		return Math.sqrt(d);
	}
	
	@Override
	public boolean equals(Object o){
		
		if (o == null)
			return false;
		
		if(!(o instanceof Matrix))
			return false;
		
		final Matrix matrix = (Matrix) o;
		
		for(int i =0; i<matrix.getm();i++){
			for(int j = 0; j<matrix.getn();j++){
				
			if(matrix.get(i, j)!=this.get(i, j))
				return false;				
			}			
		}
		return true;
		
	}
	
	@Override
	public int hashCode(){
		
		int hash = 3;
		
		for(int i =0; i<m;i++){
			for(int j = 0; j<n;j++){
				
			hash+= (int) Math.floor(this.get(i, j))*7;
				
			}			
		}		
		return hash;
		
	}

} 
