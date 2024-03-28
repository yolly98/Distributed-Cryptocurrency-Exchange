import '../css/App.css'
import { Component } from 'react'
import { Socket } from '../../utility/Socket'
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faRectangleXmark } from '@fortawesome/free-solid-svg-icons'

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
      market_operations: [],
      market_operations_limit: 20,
      pending_orders: []
    }
  }

  deleteOrder = async (order) => {
    const url = 'http://' + this.state.host + ':' + this.state.port + '/api/order'
  
    const request = {
      user: this.state.user,
      coin: this.state.coin,
      timestamp: order.key
    }
    const response = await fetch(url, {
      method : 'DELETE',
      headers: {
        'Content-type': 'application/json',
        'Accept': 'application/json'
      },
      body: JSON.stringify(request)
    })

    if (response.status != 200) {
      if (response.status == 500)
        alert('Internal server error')
      else
        alert(`Unknown error [${response.status}]`)
      return
    } 
    
    let available_assets = this.state.available_assets
    let balance = this.state.balance
    let pending_orders = [...this.state.pending_orders]

    if (order.type == 'sell') 
      available_assets += order.quantity
    else
      balance += order.quantity
    
    pending_orders = pending_orders.filter(function (pending_order) { return pending_order.key != order.key })

    this.setState({available_assets, balance, pending_orders})
  }

  update_pending_orders = (transaction) => {
    let pending_orders = [...this.state.pending_orders]

    for (let i = 0; i < pending_orders.length; i++) {
      if (transaction.order_type == pending_orders[i].type && transaction.order_timestamp == pending_orders[i].key && (transaction.seller == this.state.user || transaction.buyer == this.state.user)) {
        if (transaction.order_type == 'buy')
          pending_orders[i].quantity -= transaction.quantity * transaction.market_value
        else
          pending_orders[i].quantity -= transaction.quantity
      }
    }

    pending_orders = pending_orders.filter(function (pending_order) { return pending_order.quantity > 0 })
    this.setState({pending_orders})
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
            let date = new Date(parseInt(transaction.timestamp) / 1000000)
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

            // check the transaction list limit
            if (market_operations.length > this.state.market_operations_limit)
              market_operations.pop()

            this.update_pending_orders(transaction)
          })
        }
        this.setState({market_value, market_operations})
        break
    }
  }

  login = async () => {
    let user = document.getElementById('user-input').value
    let coin = document.getElementById('coin-input').value
    let keepalive = 45000

    // load wallet from backend
    let url = 'http://' + this.state.host + ':' + this.state.port + '/api/wallet?user=' + user + '&coin=' + coin + '&balance=true'
    let response = await fetch(url, {
      method : 'GET',
      headers: {
        'Content-type': 'application/json',
        'Accept': 'application/json'
      }
    })

    if (response.status != 200) {
      alert('Login failed')
      return
    }

    let json = await response.json()
    let balance = json.balance
    let available_assets = 0
    
    if (!Array.isArray(json.assets)) 
      available_assets = json.assets

    // load pending orders from backend
    url = 'http://' + this.state.host + ':' + this.state.port + '/api/order?user=' + user + '&coin=' + coin
    response = await fetch(url, {
      method : 'GET',
      headers: {
        'Content-type': 'application/json',
        'Accept': 'application/json'
      }
    })
    json = await response.json()

    let pending_orders = []
    if (Array.isArray(json.orders)) {
      json.orders.forEach(pending_order => {
        let date = new Date(parseInt(pending_order.timestamp) / 1000000)
        let new_pending_order = {
          key: pending_order.timestamp,
          type: pending_order.type,
          quantity: pending_order.quantity,
          timestamp: `${date.getHours()}:${date.getMinutes()}:${date.getSeconds()}`
        }
        pending_orders = [
          new_pending_order,
          ...pending_orders
        ]
      })
    }

    // open websocket to backend
    if (this.state.websocket)
      this.state.websocket.close()
    let websocket = new Socket(this.state.host, this.state.port, this.socketCallback, keepalive) 

    this.setState({balance, available_assets, websocket, user, coin, pending_orders})
  }

  operation = async (type) => {
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

    if (response.status != 200 && response.status != 500) {
      alert(`Unknown error [${response.status}]`)
      return
    } 

    const json = await response.json()
    let balance = json.balance
    let available_assets = 0

    console.log(json) // TEST
    if (!Array.isArray(json.assets)) 
      available_assets = json.asset

    if (response.status == 500) {
      if (json.quantity <= 0)
        alert('Requested operation with non positive quantity')
      else if (type == 'sell' && this.state.available_assets < quantity)
        alert('Not enough assets to sell')
      else if (type == 'buy' && this.state.balance < quantity)
        alert('Not enough money to buy')
      else
        alert('Operation failed')
    } else {
      // create new order in pending order panel
      if (!Array.isArray(json.new_pending_order)) {
        let date = new Date(parseInt(json.new_pending_order.timestamp) / 1000000)
        let new_pending_order = {
          key: json.new_pending_order.timestamp,
          type: type,
          quantity: json.new_pending_order.quantity,
          timestamp: `${date.getHours()}:${date.getMinutes()}:${date.getSeconds()}`
        }
        let pending_orders = [
          new_pending_order,
          ...this.state.pending_orders
        ]
        this.setState({pending_orders})
      }
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
          <div id='real-time-info-container'>
            <div id='transaction-list-container'>
              <h2>Market Operations</h2>
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
            <div id='order-list-container'>
            <h2>Pending Orders</h2>
              <div className='order-container'>
                <label style={{fontWeight: 'bold'}} className='order-label'>Type</label>
                <label style={{fontWeight: 'bold'}} className='order-label'>Quantity</label>
                <label style={{fontWeight: 'bold'}} className='order-label'>Time</label>
              </div>
                {
                  this.state.pending_orders.map(order => (
                    <div
                      key = {order.key}
                      className='order-container'
                    >
                      <label className='order-label'>{order.type}</label>
                      <label className='order-label'>{parseFloat(order.quantity.toFixed(6))}</label>
                      <label className='order-label'>{order.timestamp}</label>
                      <FontAwesomeIcon style={{cursor: 'pointer'}} onClick={() => this.deleteOrder(order)} icon={faRectangleXmark} />
                    </div>
                  ))
                }
            </div>
          </div>
        </div>
      </div>
    )
  }
}

export default App
