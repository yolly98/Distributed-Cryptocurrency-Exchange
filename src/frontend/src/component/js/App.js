import '../css/App.css'
import { Component } from 'react'
import { Socket } from '../../utility/Socket'

class App extends Component {

  constructor(props) {
    super(props)
    this.state = {
      balance: 0,
      available_asset: 0,
      market_value: 0,
      websocket: null
    }
  }

  socketCallback = (message) => {
    console.log("received " + JSON.stringify(message)) // TEST
    switch (message.opcode) {
      case 'new_market_value':
        let market_value = message.market_value
        this.setState({market_value})
        break
    }
  }

  login = () => {
    let user = document.getElementById('user-input').value
    let coin = document.getElementById('coin-input').value
    let keepalive = 45000
    let host = 'localhost'
    let port = '8082'
    let url = 'http://' + host + ':' + port + '/api/wallet?user=' + user + '&coin=' + coin + '&balance=true'
  
    fetch(url, {
      method : 'GET',
      headers: {
        'Content-type': 'application/json',
        'Accept': 'application/json'
      }
    }).then(
      response => response.json()
    ).then(
      json => {
        let balance = json.balance
        let available_asset = 0
        
        if (!Array.isArray(json.assets)) 
          available_asset = json.assets
        // console.log(`${balance} ${available_asset}`) // TEST

        if (this.state.websocket)
          this.state.websocket.close()
        let websocket = new Socket(host, port, this.socketCallback, keepalive) 
        this.setState({balance, available_asset, websocket})
      }
    ).catch( err => {
      console.error(err)
    })
  }



  sell = () => {
    console.log('sell')
  }

  buy = () => {
    console.log('buy')
  }

  render() {
    return (
      <div id='App'>
        <h1>Frontend</h1>

        <div id='content'>
          
          <div id='login-container'>
            <input id='user-input' type='text' placeholder='User Name'></input>
            <input id='coin-input' type='text' placeholder='Crypto Name'></input>
            <button onClick={() => this.login()}>LOGIN</button>
          </div>

          <label id='balance'>Balance: {this.state.balance}€</label>
          <label id='available-cryptos'>Assets: {this.state.available_asset}</label>

          <h2>Operations</h2>
          <label id='market-value'>Crypto Market Value: {this.state.market_value}€</label>
          <div id='op-container'>
            <div className='op'>
              <h3>Buy</h3>
              <input type='number' placeholder='Amount (euro)'></input>
              <button onClick={() => this.buy()}>BUY</button>
            </div>
            <div className='op'>
              <h3>Sell</h3>
              <input type='number' placeholder='Amount (crypto)'></input>
              <button onClick={() => this.sell()}>SELL</button>
            </div>
          </div>
        </div>
      </div>
    )
  }
}

export default App
