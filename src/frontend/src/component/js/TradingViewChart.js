import React, { Component, createRef } from 'react'
import { createChart, CrosshairMode, LineStyle, PriceScaleMode } from 'lightweight-charts'
import '../css/TradingViewChart.css'

class TradingViewChart extends Component {

  state = {
    area_series: null,
    volume_series: null
  }

  last_timestamp = null 

  shouldComponentUpdate(nextProps, nextState) {
    if (this.props.last_candlesticks != nextProps.last_candlesticks && this.props.volume_series != nextProps.last_volumes) {
      this.addNewSeries(nextProps.last_candlesticks, nextProps.last_volumes)
      return true
    }
    else
      return false
  } 

  componentDidMount() {

    const chart_options = {
      autoSize: true,
      layout: {
        background: {type: 'solid', color: '#253248'},
        textColor: 'white',
      },
      grid: {
        vertLines: {
          color: 'rgba(51, 65, 65, 0.5)',
        },
        horzLines: {
          color: 'rgba(51, 65, 65, 0.5)',
        },
      },
      crosshair: {
        mode: CrosshairMode.Normal,
        vertLine: {
          color: '#C3BCDB44',
          labelBackgroundColor: '#9B7DFF',
        },
        horzLine: {
          color: '#9B7DFF',
          labelBackgroundColor: '#9B7DFF',
        },
      },
      priceScale: {
        borderColor: "#485c7b"
      },
      leftPriceScale: {
        autoScale: true,
        mode: PriceScaleMode.Normal,
        visible: true,
        textColor: 'white'
      },
      rightPriceScale: {
        autoScale: true,
        mode: PriceScaleMode.Normal,
        visible: true,
        textColor: 'white'
      },
      timeScale: {
        timeVisible: true,
        secondVisible: false,
      }
    }
    
    const chart = createChart(document.getElementById(this.props.id).getElementsByClassName('chart')[0], chart_options)
    
    const area_series = chart.addCandlestickSeries({
      upColor: '#26a69a',
      downColor: '#ef5350', 
      borderVisible: false,
      wickUpColor: '#26a69a', 
      wickDownColor: '#ef5350',
      priceScaleId: 'right',
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
      priceScaleId: 'left',
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
    let last_timestamp = this.last_timestamp

    if (new_area_series_list.length == 1 && (!last_timestamp || new_area_series_list[0].time >= last_timestamp)) {
      area_series.update(new_area_series_list[0])
      volume_series.update(new_volume_series_list[0])
      last_timestamp = new_area_series_list[0].time
    } else {
      for (let i = 1; i < new_area_series_list.length; i++) {
        if (last_timestamp && new_area_series_list[i].time < last_timestamp)
          continue

        if (area_series && new_area_series_list)
          area_series.update(new_area_series_list[i])
    
        if (volume_series && new_volume_series_list)
          volume_series.update(new_volume_series_list[i])

        last_timestamp = new_area_series_list[i].time
      }
    }
    this.last_timestamp = last_timestamp
  }

  render() {
    // this.addNewSeries(this.props.last_candlesticks, this.props.last_volumes)
    return (
      <div className='trading-view-chart' id={this.props.id}>
        <div className='chart' ref={this.chart_container_ref}></div>
      </div>
    )
  }
}

export default TradingViewChart