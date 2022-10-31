package com.github.lzyzsd.circleprogress;

import android.content.Context;
import android.content.res.Resources;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.RectF;
import android.view.View;

/**
 * Created by bruce on 14-10-30. Edited by QuantumBadger.
 */
public class DonutProgress extends View {
	private Paint finishedPaint;
	private Paint unfinishedPaint;

	private RectF finishedOuterRect = new RectF();
	private RectF unfinishedOuterRect = new RectF();

	private boolean indeterminate;

	private float progress = 0;
	private int finishedStrokeColor;
	private int unfinishedStrokeColor;
	private int startingDegree;
	private float finishedStrokeWidth;
	private float unfinishedStrokeWidth;

	private final int min_size;

	public static float dp2px(Resources resources, float dp) {
		final float scale = resources.getDisplayMetrics().density;
		return  dp * scale + 0.5f;
	}

	public DonutProgress(Context context) {
		super(context);

		min_size = (int) dp2px(getResources(), 100);

		initPainters();
	}

	public void initPainters() {

		finishedPaint = new Paint();
		finishedPaint.setColor(finishedStrokeColor);
		finishedPaint.setStyle(Paint.Style.STROKE);
		finishedPaint.setAntiAlias(true);
		finishedPaint.setStrokeWidth(finishedStrokeWidth);

		unfinishedPaint = new Paint();
		unfinishedPaint.setColor(unfinishedStrokeColor);
		unfinishedPaint.setStyle(Paint.Style.STROKE);
		unfinishedPaint.setAntiAlias(true);
		unfinishedPaint.setStrokeWidth(unfinishedStrokeWidth);
	}

	public void setFinishedStrokeWidth(float finishedStrokeWidth) {
		this.finishedStrokeWidth = finishedStrokeWidth;
	}

	public void setUnfinishedStrokeWidth(float unfinishedStrokeWidth) {
		this.unfinishedStrokeWidth = unfinishedStrokeWidth;
	}

	public void setIndeterminate(boolean value) {
		indeterminate = value;
		invalidate();
	}

	private float getProgressAngle() {
		return getProgress() * 360f;
	}

	public float getProgress() {
		return progress;
	}

	public void setProgress(float progress) {
		if(Math.abs(progress - this.progress) > 0.0001) {
			this.progress = progress;
			invalidate();
		}
	}

	public void setFinishedStrokeColor(int finishedStrokeColor) {
		this.finishedStrokeColor = finishedStrokeColor;
	}

	public void setUnfinishedStrokeColor(int unfinishedStrokeColor) {
		this.unfinishedStrokeColor = unfinishedStrokeColor;
	}

	public int getStartingDegree() {
		return startingDegree;
	}

	@Override
	protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
		setMeasuredDimension(measure(widthMeasureSpec), measure(heightMeasureSpec));
	}

	private int measure(int measureSpec){
		int result;
		int mode = MeasureSpec.getMode(measureSpec);
		int size = MeasureSpec.getSize(measureSpec);
		if(mode == MeasureSpec.EXACTLY){
			result = size;
		}else{
			result = min_size;
			if(mode == MeasureSpec.AT_MOST){
				result = Math.min(result, size);
			}
		}
		return result;
	}

	@Override
	protected void onDraw(Canvas canvas) {
		super.onDraw(canvas);

		float delta = Math.max(finishedStrokeWidth, unfinishedStrokeWidth);
		finishedOuterRect.set(delta,
				delta,
				getWidth() - delta,
				getHeight() - delta);
		unfinishedOuterRect.set(delta,
				delta,
				getWidth() - delta,
				getHeight() - delta);

		canvas.drawArc(unfinishedOuterRect, 0, 360, false, unfinishedPaint);

		if(indeterminate) {
			final float startAngle = ((float)(System.currentTimeMillis() % 1000) * 360) / 1000;
			canvas.drawArc(finishedOuterRect, startAngle, 50, false, finishedPaint);
			invalidate();

		} else {
			canvas.drawArc(finishedOuterRect, getStartingDegree(), getProgressAngle(), false, finishedPaint);
		}
	}

	public void setStartingDegree(int startingDegree) {
		this.startingDegree = startingDegree;
	}
}
