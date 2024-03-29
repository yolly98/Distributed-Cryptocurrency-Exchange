import React, { Component } from 'react'
import { createChart, CrosshairMode, LineStyle, PriceScaleMode } from 'lightweight-charts'
import '../css/TradingViewChart.css'

class TradingViewChart extends Component {

  state = {
    area_series: null,
    volume_series: null
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
      leftPriceScale: {
        autoScale: true,
        mode: PriceScaleMode.Normal,
        visible: true
      },
      rightPriceScale: {
        autoScale: true,
        mode: PriceScaleMode.Normal,
        visible: true
      },
      timeScale: {
        timeVisible: true,
        secondVisible: false
      }
    }
    
    const chart = createChart(document.getElementById(this.props.id).getElementsByClassName('chart')[0], chartOptions)

    const area_series = chart.addCandlestickSeries({
      upColor: '#26a69a',
      downColor: '#ef5350', 
      borderVisible: false,
      wickUpColor: '#26a69a', 
      wickDownColor: '#ef5350',
    })

    area_series.priceScale().applyOptions({
      scaleMargins: {
        top: 0.1,
        bottom: 0.4,
      },
    })

    const volume_series = chart.addHistogramSeries({
      color: '#26a69a',
      priceFormat: {
        type: 'volume',
      },
      priceScaleId: '',
      scaleMargins: {
        top: 0.7,
        bottom: 0,
      },
    })

    volume_series.priceScale().applyOptions({
      scaleMargins: {
        top: 0.7,
        bottom: 0,
      },
    })

    chart.timeScale().fitContent()
    this.setState({area_series, volume_series})
    this.chart = chart
  }

  componentWillUnmount() {
    if (this.chart) {
      this.chart.remove()
    }
  }

  addNewSeries = (new_area_series_list, new_volume_series_list) => {

    let area_series = this.state.area_series
    let volume_series = this.state.volume_series

    if (new_area_series_list.length == 1) {
      area_series.update(new_area_series_list[0])
      volume_series.update(new_volume_series_list[0])
    } else {
      for (let i = 1; i < new_area_series_list.length; i++) {
        if (area_series && new_area_series_list) {
          area_series.update(new_area_series_list[i])
        }
    
        if (volume_series && new_volume_series_list) {
          volume_series.update(new_volume_series_list[i])
        }
      }
    }
  }

  render() {
    this.addNewSeries(this.props.last_candlesticks, this.props.last_volumes)
    return (
      <div className='TradingViewChart' id={this.props.id}>
        <div className='chart'></div>
      </div>
    )
  }
}

export default TradingViewChart