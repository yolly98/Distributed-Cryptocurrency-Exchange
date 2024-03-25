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
      websocket: null,
      market_operations: [
        
      ]
    }
  }

  socketCallback = (message) => {
    console.log(message) // TEST
    switch (message.opcode) {
      case 'new_placed_order':
        let market_value = message.market_value
        let market_operations = [...this.state.market_operations]

        if (message.transactions.length > 0) {
          message.transactions.forEach(transaction => {
            // create new transaction
            let date = new Date(transaction.timestamp / 1000000)
            let new_transaction = {
              key: transaction.timestamp,
              market_value: transaction.market_value,
              quantity: transaction.quantity,
              timestamp: `${date.getHours()}:${date.getMinutes()}:${date.getSeconds()}`,
              color: 'black'
            }

            // find position in the transaction list and set the color
            let i = 0
            while (i < market_operations.length) {
              if (market_operations[i].key < new_transaction.key) {
                if (new_transaction.market_value > market_operations[i].market_value)
                  new_transaction.color = 'green'
                else if (new_transaction.market_value == market_operations[i].market_value)
                  new_transaction.color = market_operations[i].color
                else
                  new_transaction.color = 'red'
                break
              }
              i++
            }

            // insert the new transaction in the transaction list
            market_operations = [
              ...market_operations.slice(0, i),
              new_transaction,
              ...market_operations.slice(i)
            ] 

            // update color of the transaction before the new one
            if (i > 0) {
              if (market_operations[i - 1].market_value > market_operations[i].market_value)
                market_operations[i - 1].color = 'green'
              else if (market_operations[i - 1].market_value == market_operations[i].market_value)
                market_operations[i - 1].color = market_operations[i].color
              else
                market_operations[i - 1].color = 'red'
            }
          })
        }
        this.setState({market_value, market_operations})
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
          <label id='market-value'>Crypto Market Value: {parseFloat(this.state.market_value.toFixed(6))}€</label>
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
          <h2>Market Operations</h2>
          <div id='transaction-list-container'>
            <div className='transaction-container'>
              <label style={{fontWeight: 'bold'}} className='transaction-label'>Price</label>
              <label style={{fontWeight: 'bold'}} className='transaction-label'>Quantity</label>
              <label style={{fontWeight: 'bold'}} className='transaction-label'>Time</label>
            </div>
            {
              this.state.market_operations.map(transaction => (
                <div
                  key = {transaction.key}
                  className='transaction-container'
                >
                  <label className='transaction-label' style={{color: transaction.color}}>{parseFloat(transaction.market_value.toFixed(6))}</label>
                  <label className='transaction-label'>{parseFloat(transaction.quantity.toFixed(6))}</label>
                  <label className='transaction-label'>{transaction.timestamp}</label>
                </div>
              ))
            }
          </div>
        </div>
      </div>
    )
  }
}

export default App
