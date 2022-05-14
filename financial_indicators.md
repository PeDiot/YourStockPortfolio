# Financial Indicators

## Momentum Indicators

### On-Balance Volume (OBV)

**Definition**

- Technical indicator of momentum, using volume changes to make price predictions
- Predict a bullish or bearish outcome

**Formula**

$$
OBV_t = OBV_{t-1} +
\begin{cases}
  \text{volume}_t \quad & \text{ if close}_t \geq \text{ close}_{t-1} \\
  0 \quad & \text{ if close}_t = \text{ close}_{t-1} \\
  - \text{volume}_t \quad & \text{ if close}_t \leq \text{ close}_{t-1} \\
\end{cases}
$$

**Interpretation**

When OBV is rising, it shows that buyers are willing to step in and push the price higher. When OBV is falling, the selling volume is outpacing buying volume, which indicates lower prices. In this way, it acts like a trend confirmation tool. If price and OBV are rising, that helps indicate a continuation of the trend.

Traders who use OBV also watch for divergence. This occurs when the indicator and price are going in different directions. If the price is rising but OBV is falling, that could indicate that the trend is not backed by strong buyers and could soon reverse.
