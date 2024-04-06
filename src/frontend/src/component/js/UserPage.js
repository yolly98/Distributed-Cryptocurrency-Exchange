import React, {Component} from 'react'
import { Link } from 'react-router-dom'
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faRightFromBracket } from '@fortawesome/free-solid-svg-icons'
import '../css/UserPage.css'

class UserPage extends Component {

  state = {
    user: null,
    balance: null,
    assets: [],
    coins: []
  }

  formatQuantity = (quantity) => {
    if (!quantity)
      return '0'
    quantity = quantity.toFixed(6)
    if (parseFloat(quantity) == 0)
      return `~0`
    return quantity
  }

  componentDidMount() {
    let user = sessionStorage.getItem('user')
    if (!user) {
      window.location.href = '/login'
      return
    }
    
    // load configuration
    fetch('/config.json')
    .then(response => response.json())
    .then(config => {
      this.setState({
        user,
        host: config.host,
        port: config.port
      }, () => this.initialize())
    })
    .catch(error => console.error(error));
  }

  initialize = async () => {
    let user = this.state.user

    // load wallet from backend
    let url = 'http://' + this.state.host + ':' + this.state.port + '/api/wallet?user=' + user + '&coin=all&balance=true'
    let response = await fetch(url, {
      method : 'GET',
      headers: {
        'Accept': 'application/json'
      }
    })

    if (response.status != 200) {
      alert('Load wallet failed')
      return
    }

    let json = await response.json()
    let balance = json.balance
    let assets = []
    
    if (Array.isArray(json.assets)) { 
      assets = json.assets
    }

    // load all coins
    let coins = []
    url = 'http://' + this.state.host + ':' + this.state.port + '/api/coin'
    response = await fetch(url, {
      method : 'GET',
      headers: {
        'Accept': 'application/json'
      }
    })

    if (response.status != 200) {
      alert('Load coins failed')
      return
    }

    json = await response.json()
    if (Array.isArray(json.coins)) { 
      coins = json.coins
    }

    this.setState({
      balance,
      assets,
      coins
    })
  }

  deposit = async (operation) => {
    let coin = document.getElementById('user-page-coins-select').value
    let value = document.getElementById('user-page-coin-input').value

    if (value == '') {
      alert('Quantity can\'t be empty')
      return
    }

    let url = 'http://' + this.state.host + ':' + this.state.port + '/api/wallet'
    let request = {
      user: this.state.user,
      type: coin,
      operation: operation,
      quantity: parseFloat(value)
    }
    let response = await fetch(url, {
      method : 'PUT',
      headers: {
        'Content-type': 'application/json',
        'Accept': 'application/json'
      },
      body: JSON.stringify(request)
    })

    if (response.status != 200) {
      alert('Operation failed')
      return
    }

    this.initialize()
  }

  render() {

    return(
      <div id='user-page'>
        <div id='user-page-header'>
          <select name='coins' id='user-page-goto-select' defaultValue={''}>
              {
                this.state.coins.map(coin => (
                  <option key={coin.coin} value={coin.coin}>{coin.coin}</option>
                ))
              }
          </select>
          <button id='user-page-goto-link' onClick={() => {window.location.href = `trade/${document.getElementById('user-page-goto-select').value}`}}>Go To</button>
          <Link id='user-page-exit-button' to='/login'>
            <FontAwesomeIcon style={{cursor: 'pointer'}} icon={faRightFromBracket} />
          </Link>
        </div>
        <h1>{this.state.user}</h1>
        <div id='user-page-container'>
          <h2>My Balance</h2>
          <label id='user-page-balance-label'>{this.formatQuantity(this.state.balance)}€</label>
          <div id='user-page-wallet-container'>
            <div id='user-page-wallet-input-container'>
              <select name='coins' id='user-page-coins-select' defaultValue={'euro'}>
                <option value='deposit'>euro</option>
                {
                  this.state.coins.map(coin => (
                    <option key={coin.coin} value={coin.coin}>{coin.coin}</option>
                  ))
                }
              </select>
              <input type='number' id='user-page-coin-input' placeholder='Quantity'></input>
            </div> 
            <div id='user-page-wallet-button'>
              <button onClick={() => this.deposit('add')}>DEPOSIT</button>
              <button onClick={() => this.deposit('sub')}>WITHDRAW</button>
            </div>
          </div>
          <div id='user-page-assets'>
            <h2>My Assets</h2>
            <div id='user-page-assets-list'>  
              <div className='user-page-asset-container-title'>
                <label style={{fontWeight: 'bold'}}>Coin</label>
                <label style={{fontWeight: 'bold'}}>Quantity</label>
              </div>
              {
              this.state.assets.map(asset => (
                <Link key={asset.coin} to={`/trade/${asset.coin}`} className='user-page-asset-container'>
                  <label>{asset.coin}</label>
                  <label>{this.formatQuantity(asset.quantity)}</label>
                </Link>
              )) 
              }
            </div>    
          </div>
        </div>
      </div>
    )
  }
}

export default UserPage