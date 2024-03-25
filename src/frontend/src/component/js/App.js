import '../css/App.css'
import { Component } from 'react'
import { Socket } from '../../utility/Socket'

class App extends Component {

  constructor(props) {
    super(props)
    this.state = {
      user: null,
      host: 'localhost',
      port: 8082,
      coin: null,
      balance: 0,
      available_assets: 0,
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
    const url = 'http://' + this.state.host + ':' + this.state.port + '/api/wallet?user=' + user + '&coin=' + coin + '&balance=true'
  
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
        let available_assets = 0
        
        if (!Array.isArray(json.assets)) 
          available_assets = json.assets
        // console.log(`${balance} ${available_assets}`) // TEST

        if (this.state.websocket)
          this.state.websocket.close()
        let websocket = new Socket(this.state.host, this.state.port, this.socketCallback, keepalive) 
        this.setState({balance, available_assets, websocket, user, coin})
      }
    ).catch( err => {
      console.error(err)
    })
  }

  operation = async (type) => {
    console.log(type)
    let quantity = parseFloat(document.getElementById(type + '-input').value)
    const url = 'http://' + this.state.host + ':' + this.state.port + '/api/order'
  
    const request = {
      opcode: type,
      user: this.state.user,
      coin: this.state.coin,
      quantity: quantity
    }
    const response = await fetch(url, {
      method : 'POST',
      headers: {
        'Content-type': 'application/json',
        'Accept': 'application/json'
      },
      body: JSON.stringify(request)
    })

    if (response.status != 200) {
      alert('Internal server error')
      return
    } 

    const json = await response.json()
    let balance = json.balance
    let available_assets = 0

    if (!Array.isArray(json.assets)) 
      available_assets = json.asset

    if (json.state == 'failed') {
      if (type == 'sell' && this.state.available_assets < quantity)
        alert('Not enough assets to sell')
      else if (type == 'buy' && this.state.balance < quantity)
        alert('Not enough money to buy')
      else
        alert('Operation failed')
    }

    this.setState({balance, available_assets})
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
          <label id='available-cryptos'>Assets: {this.state.available_assets}</label>

          <h2>Operations</h2>
          <label id='market-value'>Crypto Market Value: {this.state.market_value}€</label>
          <div id='op-container'>
            <div className='op'>
              <h3>Buy</h3>
              <input id='buy-input' type='number' placeholder='Amount (euro)'></input>
              <button onClick={() => this.operation('buy')}>BUY</button>
            </div>
            <div className='op'>
              <h3>Sell</h3>
              <input id='sell-input' type='number' placeholder='Amount (crypto)'></input>
              <button onClick={() => this.operation('sell')}>SELL</button>
            </div>
          </div>
        </div>
      </div>
    )
  }
}

export default App
