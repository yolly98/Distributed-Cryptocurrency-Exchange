import React, { Component } from 'react'
import { createChart, CrosshairMode, LineStyle } from 'lightweight-charts'
import '../css/TradingViewChart.css'

class TradingViewChart extends Component {

  state = {
    candlestickSeries: null
  }

  componentDidMount() {
    const chartOptions = {
      layout: { textColor: 'black', background: { type: 'solid', color: 'white' } },
      width: 700,
      height: 500,
      crosshair: {
        mode: CrosshairMode.Normal,
        vertLine: {
          width: 8,
          color: '#C3BCDB44',
          style: LineStyle.Solid,
          labelBackgroundColor: '#9B7DFF',
        },
        horzLine: {
          color: '#9B7DFF',
          labelBackgroundColor: '#9B7DFF',
        },
      },
      timeScale: {
        timeVisible: true,
        secondVisible: false
      }
    }
    
    const chart = createChart(document.getElementById(this.props.id).getElementsByClassName('chart')[0], chartOptions)

    const series = chart.addCandlestickSeries({
      upColor: '#26a69a', downColor: '#ef5350', borderVisible: false,
      wickUpColor: '#26a69a', wickDownColor: '#ef5350',
    })

    chart.timeScale().fitContent()
    this.setState({ candlestickSeries: series })
    this.chart = chart
  }

  componentWillUnmount() {
    if (this.chart) {
      this.chart.remove()
    }
  }

  addNewSeries = (series) => {
    const candlestickSeries = this.state.candlestickSeries;
    if (candlestickSeries && series) {
      candlestickSeries.update(series)
    }
  }

  render() {
    this.addNewSeries(this.props.last_candlestick)
    return (
      <div className="TradingViewChart" id={this.props.id}>
        <div className='chart'></div>
      </div>
    );
  }
}

export default TradingViewChart